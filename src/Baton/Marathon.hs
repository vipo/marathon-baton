{-# LANGUAGE OverloadedStrings #-}
module Baton.Marathon(
  dockerApps
) where

import qualified Baton.Types as T

import Control.Lens

import Data.Aeson
import Data.Aeson.Types

import qualified Data.Maybe as M

import Network.Wreq

data Docker = Docker {
  image :: String
} deriving Show

instance FromJSON Docker where
  parseJSON (Object v) = Docker <$> v .: "image"
  parseJSON wat        = typeMismatch "Docker" wat

data Container = Container {
    containerType :: String
  , docker :: Maybe Docker
} deriving Show

instance FromJSON Container where
  parseJSON (Object v) = Container <$> v .: "type" <*> v .: "docker"
  parseJSON wat        = typeMismatch "Container" wat

data App = App {
    appId :: String
  , appContainer :: Maybe Container
} deriving Show

instance FromJSON App where
  parseJSON (Object v) = App <$> v .: "id" <*> v .: "container"
  parseJSON wat        = typeMismatch "App" wat

data Apps = Apps {
    apps :: [App]
} deriving Show

instance FromJSON Apps where
  parseJSON (Object v) = Apps <$> v .: "apps"
  parseJSON wat        = typeMismatch "Apps" wat

dockerApps :: String           -- ^ Marathon api url
           -> IO [T.MarathonApp] -- ^ Currently running apps
dockerApps url = do
  r <- asJSON =<< get (url ++ "/v2/apps") :: IO (Response Apps)
  let appList = apps $ r ^. responseBody
  let containerList = [(appId a, c) | a <- appList, c <- M.maybeToList (appContainer a)]
  let dockerList = [ (n, image d) | (n, c) <- containerList, containerType c == "DOCKER", d <- M.maybeToList (docker c)]
  return $ map (\(n, i) -> T.MarathonApp n i) dockerList
