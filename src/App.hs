{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App
  ( App(..)
  , Env(..)
  , MonadLogger
  , info
  ) where

import           Control.Monad.Reader           ( ask
                                                , MonadReader
                                                , ReaderT
                                                )
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Data.Text                      ( Text
                                                )

data Env
  = Env
  { envPort :: Int
  , envLog :: Text -> IO ()
  }

newtype App a
  = App
  { runApp :: ReaderT Env IO a
  }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Env
  , MonadIO
  )

class HasLogger m a where
  getLogger :: a -> Text -> m ()

instance HasLogger IO Env where
  getLogger = envLog

class Monad m => MonadLogger m where
  info :: Text -> m ()

instance MonadLogger App where
  info text = do
    env <- ask
    liftIO $ envLog env text
