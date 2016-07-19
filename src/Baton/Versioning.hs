module Baton.Versioning (
    versionDockerImage
) where

import Baton.Types

import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Maybe as M

versionDockerImage :: [String]          -- ^ local registries
                   -> String            -- ^ full image
                   -> Maybe DockerImage -- ^ parsed entity
versionDockerImage registries image =
  let
    prefixed = map (\r -> fmap (\suff -> (r, suff)) (L.stripPrefix r image)) registries
    asList = prefixed >>= M.maybeToList
    splitTagImage s = case S.splitOn ":" s of
      h : t : [] -> Just (h, t)
      h : []     -> Just (h, "latest")
      _          -> Nothing
  in
    case asList of
      ((r, '/': i) : _) -> splitTagImage i >>= (\(n, t) -> Just $ DockerImage n r t)
      _                 -> Nothing
