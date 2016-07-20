module Baton.Versioning (
    versionDockerImage
  , orderVersions
) where

import Baton.Types

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Maybe as M
import qualified Data.Ord as O
import qualified Text.Read as T

versionDockerImage :: [String]          -- ^ local registries
                   -> String            -- ^ full image
                   -> Maybe DockerImage -- ^ parsed entity
versionDockerImage registries image =
  let
    prefixed = map (\r -> fmap (\suff -> (r, suff)) (L.stripPrefix r image)) registries
    asList = prefixed >>= M.maybeToList
    splitTagImage s = case S.splitOn ":" s of
      [h, t] -> Just (h, t)
      [h]    -> Just (h, "latest")
      _      -> Nothing
  in
    case asList of
      ((r, '/': i) : _) -> splitTagImage i >>= (\(n, t) -> Just $ DockerImage n r t)
      _                 -> Nothing

orderVersions :: [String]  -- ^ versions to order
              -> [String]  -- ^ ordered (somehow) versions
orderVersions vers =
  let
    gs :: String -> [Maybe Integer]
    gs v = map T.readMaybe $ L.groupBy (\a b -> C.isDigit a && C.isDigit b) v
    extractNumbers v = gs v >>= M.maybeToList
  in
    L.sortBy (flip (O.comparing extractNumbers)) vers
