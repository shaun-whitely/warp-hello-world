module DB
  ( MonadDatabase(..)
  ) where

import           Control.Monad.IO.Class     ( liftIO
                                            )
import           Control.Monad.Reader       ( asks
                                            )
import           Database.PostgreSQL.Simple ( ToRow
                                            , FromRow
                                            , Query
                                            )
import           Data.Int                   ( Int64
                                            )
import           Data.Pool                  ( withResource
                                            )
import qualified Database.PostgreSQL.Simple as PG
import           App

class Monad m => MonadDatabase m where
  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  query_ :: FromRow r => Query -> m [r]
  execute :: ToRow q => Query -> q -> m Int64
  execute_ :: Query -> m Int64

instance MonadDatabase App where
  query q params = do
    pool <- asks envPool
    liftIO $ withResource pool $ \conn ->
      PG.query conn q params
  
  query_ q = do
    pool <- asks envPool
    liftIO $ withResource pool $ \conn ->
      PG.query_ conn q

  execute q params = do
    pool <- asks envPool
    liftIO $ withResource pool $ \conn ->
      PG.execute conn q params

  execute_ q = do
    pool <- asks envPool
    liftIO $ withResource pool $ \conn ->
      PG.execute_ conn q
