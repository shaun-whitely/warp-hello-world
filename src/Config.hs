{-# LANGUAGE TemplateHaskell #-}
module Config
  ( Config
  , mkConfig
  , port
  , dbConfig
  , DbConfig
  , mkDbConfig
  , host
  , user
  , password
  , database
  , fromEnvironment
  ) where

import           Control.Lens
import           System.Environment             ( getEnv
                                                )

data DbConfig
  = DbConfig
  { _host :: String
  , _user :: String
  , _password :: String
  , _database :: String
  }
  deriving (Show)

mkDbConfig
  :: String -- host
  -> String -- user
  -> String -- password
  -> String -- database
  -> DbConfig
mkDbConfig = DbConfig

makeLenses ''DbConfig

data Config
  = Config
  { _port :: Int
  , _dbConfig :: DbConfig
  }
  deriving (Show)

mkConfig
  :: Int
  -> DbConfig
  -> Config
mkConfig = Config

makeLenses ''Config

fromEnvironment :: IO Config
fromEnvironment =
  let
    dbHost = getEnv "DB_HOST"
    dbUser = getEnv "DB_USER"
    dbPassword = getEnv "DB_PASSWORD"
    dbDatabase = getEnv "DB_DATABASE"
    dbConfig = mkDbConfig <$> dbHost <*> dbUser <*> dbPassword <*> dbDatabase
    port = read <$> getEnv "PORT"
  in
    mkConfig <$> port <*> dbConfig
