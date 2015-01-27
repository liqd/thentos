{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS  #-}

module DB.Protect
  ( mkThentosClearance
  , thentosLabeledPublic
  , thentosLabeledDenied
  , thentosAllClear
  , thentosDenied
  , allowEverything
  , allowNothing
  , godCredentials
  , createGod
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Either (isLeft, isRight)
import Data.String.Conversions (ST)
import LIO.DCLabel (dcPublic, (%%))
import Network.HTTP.Types.Header (Header)

import Types
import DB.Error
import DB.Core


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
authenticateUser db name password = if verifyUserPassword db name password
    then Right allowEverything  -- FIXME: construct proper privileges here.
    else Left BadCredentials

verifyUserPassword :: DB -> UserName -> UserPass -> Bool
verifyUserPassword db name password =
    maybe False ((password ==) . (^. userPassword) . snd) $
        pure_lookupUserByName db name


authenticateService :: DB -> ServiceId -> ServiceKey -> Either DbError ThentosClearance
authenticateService _ _ _ = Right allowEverything  -- FIXME


allowNothing :: ThentosClearance
allowNothing = ThentosClearance (False %% True)
  -- FIXME: is this correct?  what does it meaen?

allowEverything :: ThentosClearance
allowEverything = ThentosClearance dcPublic


godCredentials :: [Header]
godCredentials = [("X-Thentos-User", "god"), ("X-Thentos-Password", "god")]

createGod :: AcidState DB -> Bool -> IO ()
createGod st verbose = do
    eq <- query' st (LookupUser (UserId 0) thentosAllClear)
    when (isLeft eq) $ do
        when verbose $
            putStr "No users.  Creating god user with password 'god'... "
        eu <- update' st (AddUser (User "god" "god" "god@home" [] []) thentosAllClear)
        when verbose $
            if isRight eu
                then putStrLn "[ok]"
                else putStrLn $ "[failed: " ++ show eu ++ "]"
