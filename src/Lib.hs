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
                                            , requestMethod
                                            , pathInfo
                                            )
import           Network.Wai.Handler.Warp   ( run
                                            )
import           Network.HTTP.Types         ( status200
                                            , status404
                                            )

import           App
import           Config
import           Controller
import           DB

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
  config <- fromEnvironment
  pool <- createDbConnectionPool $ config^.dbConfig
  let env = Env { envLog = T.putStrLn, envPool = pool }
  putStrLn $ "Listening on port " <> (show (config^.port))
  run (config^.port) $ app env

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
handleRequest req =
  case (requestMethod req, pathInfo req) of
    ("GET", ["count"])  -> getCount
    ("POST", ["count"]) -> incrementCount
    _                   -> pure $ responseLBS status404 [] "Unknown Route"
