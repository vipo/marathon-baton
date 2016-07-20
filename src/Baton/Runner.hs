{-# LANGUAGE OverloadedStrings #-}

module Baton.Runner(
  run
) where

import           Baton.Types

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           System.Exit
import           System.Process

run :: String                        -- ^ path to executable
    -> DockerApp                     -- ^ docker app
    -> IO (ExitCode, String, String) -- ^ result, stdout, stderr
run path (DockerApp n (DockerImage i r t)) =
  readProcessWithExitCode path [n, r ++ "/" ++ i, t] ""
