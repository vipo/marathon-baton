{-# LANGUAGE OverloadedStrings #-}

module Baton.Runner(
  run
) where

import           Baton.Types

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           Data.UUID
import           Data.UUID.V4

import           System.Directory
import           System.Exit
import           System.IO
import           System.Process

run :: Configuration  -- ^ configuration
    -> DockerApp      -- ^ docker app
    -> IO String      -- ^ uuid of run
run conf (DockerApp n (DockerImage i r t)) = do
  uuid <- fmap toString nextRandom
  let dir = workingDir conf ++ "/runs/" ++ uuid
  createDirectoryIfMissing True dir
  stdout <- openFile (dir ++ "/stdout") WriteMode
  hSetBuffering stdout NoBuffering
  stderr <- openFile (dir ++ "/stderr") WriteMode
  hSetBuffering stderr NoBuffering
  (_, _, _, ph) <-
      createProcess (proc (pathToExecutable conf) [n, r ++ "/" ++ i, t]) {
          std_out = UseHandle stdout
        , std_err = UseHandle stderr
        , cwd = Just dir
      }
  return uuid
