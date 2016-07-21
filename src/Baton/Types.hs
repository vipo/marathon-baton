module Baton.Types
where

data Configuration = Configuration {
    marathonUrl :: String
  , dockerRegistries :: [String]
  , workingDir :: String
  , pathToExecutable :: String
} deriving Show

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
