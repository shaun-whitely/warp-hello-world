{-# LANGUAGE OverloadedStrings #-}
module Controller
  ( getCount
  , incrementCount
  ) where

import           Data.Maybe                 ( listToMaybe
                                            )
import           Data.Text.Lazy             ( pack
                                            )
import           Data.Text.Lazy.Encoding    ( encodeUtf8
                                            )
import           Database.PostgreSQL.Simple ( fromOnly
                                            , Only
                                            , Query(..)
                                            )
import           Network.HTTP.Types         ( status200
                                            , status500
                                            )
import qualified Network.Wai                as W
import           DB

getCount
  :: (MonadDatabase m)
  => m W.Response
getCount = do
  counts <- (fmap.fmap) fromOnly queryCount
  case listToMaybe counts of
    Just count -> 
      pure $ W.responseLBS status200 [] (encodeUtf8 . pack . show $ count)
    Nothing ->
      pure $ W.responseLBS status500 [] "Could not get count"

queryCount
  :: MonadDatabase m
  => m [Only Int]
queryCount = query_ "SELECT count FROM count"

incrementCount
  :: (MonadDatabase m)
  => m W.Response
incrementCount = do
  execute_ "UPDATE count SET count = count + 1"
  pure $ W.responseLBS status200 [] "Success\n"
