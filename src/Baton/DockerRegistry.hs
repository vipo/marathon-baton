{-# LANGUAGE OverloadedStrings #-}
module Baton.DockerRegistry (
    listTags
)where

import qualified Baton.Types as T

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Maybe as M

import           Network.Wreq

data Tags = Tags {
    name :: String
  , tags :: [String]
} deriving Show

instance FromJSON Tags where
  parseJSON (Object v) = Tags <$> v .: "name" <*> v .: "tags"
  parseJSON wat        = typeMismatch "Tags" wat

listTags :: T.Configuration -- ^ app conf
         -> String          -- ^ docker registry
         -> String          -- ^ image name
         -> IO [String]     -- ^ tags
listTags conf registry image = do
  let url = "https://" ++ registry ++ "/v2/" ++ image ++ "/tags/list"
  r <- asJSON =<< getWith (T.wreqOptions conf) url :: IO (Response Tags)
  return $ tags $ r ^. responseBody
