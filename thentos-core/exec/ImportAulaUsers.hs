{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Control.Monad (forM, void)
import Data.Csv (decode, HasHeader(NoHeader))
import Data.Text (Text)
import Data.Void (Void)
import LIO.DCLabel (toCNF)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Thentos.Types
import Thentos.Action.Types (Action, ActionState)
import Thentos.Action (addUserWithTempPassword)
import Thentos.Action.Core
import Thentos.Config
import Thentos (makeActionState, createConnPoolAndInitDb)

userDataPath :: FilePath
userDataPath = "users.csv"

main :: IO ()
main = do
    config :: ThentosConfig <- getConfig "devel.config"
    connPool <- createConnPoolAndInitDb config
    actionState :: ActionState <- makeActionState config connPool
    fileContent <- BL.readFile userDataPath
    case decode NoHeader fileContent of
        Left _ -> error $ "could not parse " ++ userDataPath
        Right userData -> do
            let userDataList :: [(Text, Maybe UserEmail)] = V.toList userData
            void $ runActionWithPrivs [toCNF RoleAdmin] () actionState
                                      (importUsers userDataList :: Action Void () [(UserId, UserPass)])

dummyEmail :: UserEmail
dummyEmail = (\(Just e) -> e) $ parseUserEmail "dummy@example.com"

importUsers :: [(Text, Maybe UserEmail)] -> Action e s [(UserId, UserPass)]
importUsers users =
    forM users $ \(name, mEmail) -> do
        let email = case mEmail of
                Just e -> e
                Nothing -> dummyEmail

        addUserWithTempPassword (UserName name) email
