{-# LANGUAGE OverloadedStrings #-}

module Baton.Runner(
  run
) where

import           Baton.Types

import           Control.Concurrent

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
  let dir = workingDir conf ++ "/run/" ++ uuid
  createDirectoryIfMissing True dir
  writeStatus dir "Running"
  stdout <- openFile (dir ++ "/stdout.txt") WriteMode
  hSetBuffering stdout NoBuffering
  stderr <- openFile (dir ++ "/stderr.txt") WriteMode
  hSetBuffering stderr NoBuffering
  (_, _, _, ph) <-
      createProcess (proc (pathToExecutable conf) [n, r ++ "/" ++ i, t]) {
          std_out = UseHandle stdout
        , std_err = UseHandle stderr
        , cwd = Just dir
      }
  forkIO $ waitForCompletion dir ph
  return uuid

writeStatus dir =
    writeFile (dir ++ "/status.txt")

waitForCompletion dir ph = do
  status <- waitForProcess ph
  writeStatus dir (show status)
