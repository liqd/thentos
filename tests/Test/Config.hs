{-# OPTIONS  #-}

module Test.Config
where

data Config =
    Config
      { dbPath :: FilePath
      , restPort :: Int
      , serverFullBackendPort :: Int
      , serverFullFrontendPort :: Int
      , webdriverHost :: String
      , webdriverPort :: Int
      }
  deriving (Eq, Show)

config :: Config
config =
    Config
      { dbPath = ".test-db/"
      , restPort = 8002
      , serverFullBackendPort = 7118
      , serverFullFrontendPort = 7119
      , webdriverHost = "localhost"
      , webdriverPort = 4451
      }
