{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns  #-}
module Main where

import qualified Baton.Types as T
import qualified Baton.Controller as C

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Concurrent
import qualified Control.Exception as E
import           Data.Monoid        (mconcat)
import           Data.List.Split    (endBy)
import           System.Environment
import           System.Exit
import           System.Directory
import           System.Posix.Signals
import           Web.Scotty.Trans

main :: IO ()
main = do
  tid <- myThreadId
  installHandler sigTERM (Catch (E.throwTo tid ExitSuccess)) Nothing
  marathonUrl <- getEnv "MARATHON_URL"
  registries <- endBy "," <$> getEnv "DOCKER_REGISTRIES"
  args <- getArgs
  dir <- getCurrentDirectory
  let !conf = case args of
        [path] -> T.Configuration marathonUrl registries dir path
        _      -> error "Please provide path to deployment execuable"
  let reader r = runReaderT r conf
  scottyT 3000 reader C.routes
