{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS  #-}

module DB.Protect
  ( mkThentosClearance
  , allowEverything
  , allowReadEverything
  , allowNothing
  , (*%%)
  , createDefaultUser
  ) where

import Control.Monad (when)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Data.List (foldl')
import Data.String.Conversions (ST)
import LIO.DCLabel (ToCNF, CNF, toCNF, (%%), (/\), (\/))
import System.Log (Priority(DEBUG, ERROR))
import Text.Show.Pretty

import DB.Core
import DB.Trans
import System.Log.Missing (logger)
import Types
import Util


-- | If password cannot be verified, or if only password or only
-- principal is provided, throw an error explaining the problem.  If
-- none are provided, set clearance level to 'allowNothing'.  If both
-- are provided, look up roles of principal, and set clearance level
-- to that of the principal aka agent and all its roles.
--
-- Note: Both 'Role's and 'Agent's can be used in authorization
-- policies.  ('User' can be used, but it must be wrapped into an
-- 'UserA'.)
mkThentosClearance :: Maybe ST -> Maybe ST -> Maybe ST -> DB -> Either DbError ThentosClearance
mkThentosClearance (Just user) Nothing        (Just password) db = authenticateUser db (UserName user) (textToPassword password)
mkThentosClearance Nothing     (Just service) (Just password) db = authenticateService db (ServiceId service) (ServiceKey password)
mkThentosClearance Nothing     Nothing        Nothing         _  = Right allowNothing
mkThentosClearance _           _              _               _  = Left BadAuthenticationHeaders


authenticateUser :: DB -> UserName -> UserPass -> Either DbError ThentosClearance
authenticateUser db name password = do
    (uid, user) :: (UserId, User)
        <- maybe (Left BadCredentials) Right $ pure_lookupUserByName db name

    credentials :: [CNF]
        <- let a = UserA uid
           in Right $ toCNF a : map toCNF (pure_lookupAgentRoles db a)

    if verifyPass password user
        then Right $ simpleClearance credentials
        else Left BadCredentials


authenticateService :: DB -> ServiceId -> ServiceKey -> Either DbError ThentosClearance
authenticateService db sid keyFromClient = do
    (_, Service keyFromDb)
        <- maybe (Left BadCredentials) (Right) $ pure_lookupService db sid

    credentials :: [CNF]
        <- let a = ServiceA sid
           in Right $ toCNF a : map toCNF (pure_lookupAgentRoles db a)

    if keyFromClient /= keyFromDb
        then Left BadCredentials
        else Right $ simpleClearance credentials


simpleClearance :: [CNF] -> ThentosClearance
simpleClearance credentials = case credentials of
    []     -> allowNothing
    (x:xs) -> foldl' (/\) x xs *%% foldl' (\/) x xs


-- | Clearance for everything.
allowEverything :: ThentosClearance
allowEverything = False *%% True

-- | Clearance to read everything, but write / create nothing.
allowReadEverything :: ThentosClearance
allowReadEverything = False *%% False

-- | Clearance only for things labeled 'thentosPublic'.
allowNothing :: ThentosClearance
allowNothing = True *%% False


(*%%) :: (ToCNF a, ToCNF b) => a -> b -> ThentosClearance
(*%%) a b = ThentosClearance $ a %% b
infix 6 *%%


-- * default user from config file

-- | If default user is 'Nothing' or user with 'UserId 0' exists, do
-- nothing.  Otherwise, create default user.
--createDefaultUser :: AcidState DB -> Maybe (User, [Role]) -> IO ()
createDefaultUser :: AcidState DB -> Maybe (UserFormData, [Role]) -> IO ()
createDefaultUser _ Nothing = return ()
createDefaultUser st (Just (userData, roles)) = do
    eq <- query' st (LookupUser (UserId 0) allowEverything)
    when (isLeft eq) $ do
        -- user
        user <- makeUserFromFormData userData
        logger DEBUG $ "No users.  Creating default user: " ++ ppShow (UserId 0, user)
        eu <- update' st (AddUser user allowEverything)

        if eu == Right (UserId 0)
            then logger DEBUG $ "[ok]"
            else logger ERROR $ "failed to create default user: " ++ ppShow (UserId 0, user)

        -- roles
        logger DEBUG $ "Adding default user to roles: " ++ ppShow roles
        result <- mapM (\ role -> update' st (AssignRole (UserA (UserId 0)) role allowEverything)) roles

        if all isRight result
            then logger DEBUG $ "[ok]"
            else logger ERROR $ "failed to assign default user to roles: " ++ ppShow (UserId 0, user, roles)
