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
  , godCredentials
  , createGod
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Data.List (foldl')
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Data.String.Conversions (ST)
import LIO.DCLabel (ToCNF, CNF, toCNF, (%%), (/\), (\/))
import Network.HTTP.Types.Header (Header)

import DB.Api
import DB.Core
import Types


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
mkThentosClearance (Just user) Nothing        (Just password) db = authenticateUser db (UserName user) (UserPass password)
mkThentosClearance Nothing     (Just service) (Just password) db = authenticateService db (ServiceId service) (ServiceKey password)
mkThentosClearance Nothing     Nothing        Nothing         _  = Right allowNothing
mkThentosClearance _           _              _               _  = Left BadAuthenticationHeaders


authenticateUser :: DB -> UserName -> UserPass -> Either DbError ThentosClearance
authenticateUser db name password = do
    (uid, user) :: (UserId, User)
        <- maybe (Left BadCredentials) (Right) $ pure_lookupUserByName db name

    credentials :: [CNF]
        <- let a = UserA uid
           in Right $ toCNF a : map toCNF (pure_lookupAgentRoles db a)

    if user ^. userPassword /= password
        then Left BadCredentials
        else Right $ simpleClearance credentials


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


-- * god user (do not use in production)

godCredentials :: [Header]
godCredentials = [("X-Thentos-User", "god"), ("X-Thentos-Password", "god")]

createGod :: AcidState DB -> Bool -> IO ()
createGod st verbose = do
    eq <- query' st (LookupUser (UserId 0) allowEverything)
    when (isLeft eq) $ do
        -- user
        when verbose $
            putStr "No users.  Creating god user with password 'god'... "
        eu <- update' st (AddUser (User "god" "god" "god@home" [] []) allowEverything)
        when verbose $
            if eu == Right (UserId 0)
                then putStrLn "[ok]"
                else putStrLn $ "[failed: " ++ show eu ++ "]"

        -- roles
        when verbose $
            putStr "Adding user 'god' to roles 'Admin', 'User'... "
        result <- update' st (AssignRole (UserA (UserId 0)) RoleAdmin allowEverything)
        when verbose $
            if isRight result
                then putStrLn "[ok]"
                else putStrLn $ "[failed: " ++ show result ++ "]"
