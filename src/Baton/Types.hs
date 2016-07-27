module Baton.Types
where

import qualified Network.Wreq as W

data Configuration = Configuration {
    marathonUrl :: String
  , dockerRegistries :: [String]
  , workingDir :: String
  , pathToExecutable :: String
  , wreqOptions :: W.Options
}

data MarathonApp = MarathonApp {
    appName :: String
  , dockerImage :: String
} deriving Show

data DockerImage = DockerImage {
    name :: String
  , registry :: String
  , tag :: String
} deriving Show

data DockerApp = DockerApp {
    appId :: String
  , docker :: DockerImage
} deriving Show
