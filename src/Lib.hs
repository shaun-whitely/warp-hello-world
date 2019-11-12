{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( runApplication
  ) where

import           Control.Lens
import           Control.Monad.Reader       ( runReaderT
                                            )
import           Control.Monad.IO.Class     ( liftIO
                                            )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Pool                  ( Pool
                                            , createPool
                                            )
import qualified Database.PostgreSQL.Simple as PG
import           Network.Wai                ( Application
                                            , Request
                                            , Response
                                            , ResponseReceived
                                            , responseLBS
                                            )
import           Network.Wai.Handler.Warp   ( run
                                            )
import           Network.HTTP.Types         ( status200
                                            )

import           App
import           Config

-- TODO: Use pool
createDbConnectionPool :: DbConfig -> IO (Pool PG.Connection)
createDbConnectionPool dbConfig =
  let
    connectInfo
      = PG.defaultConnectInfo
      { PG.connectHost = dbConfig^.host
      , PG.connectUser = dbConfig^.user
      , PG.connectPassword = dbConfig^.password
      , PG.connectDatabase = dbConfig^.database
      }
    createResource = PG.connect connectInfo
    destroyResource = PG.close
    stripes = 1
    keepalive = 30
    size = 5
  in
    createPool createResource destroyResource stripes keepalive size

runApplication :: IO ()
runApplication = do
  let env = Env { envPort = 8080, envLog = T.putStrLn }
  putStrLn $ "Listening on port " <> (show (envPort env))
  run (envPort env) $ app env

app
  :: Env
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
app env req cb = do
  resp <- runReaderT (runApp $ handleRequest req) env
  cb resp

handleRequest
  :: Request
  -> App Response
handleRequest _ = do
  info "Incoming request"
  pure $ responseLBS status200 [] "Hello, world!\n"
