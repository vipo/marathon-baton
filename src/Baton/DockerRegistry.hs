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

listTags :: String      -- ^ docker registry
         -> String      -- ^ image name
         -> IO [String] -- ^ tags
listTags registry image = do
  let url = "https://" ++ registry ++ "/v2/" ++ image ++ "/tags/list"
  r <- asJSON =<< get url :: IO (Response Tags)
  return $ tags $ r ^. responseBody
