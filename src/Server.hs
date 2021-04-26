{-# LANGUAGE OverloadedStrings #-}


module Main where


import Api
import Automate
import Config
import Types
import Types.Api

import Data.List.Extra (trim)
import Network.Wai.Handler.Warp (run)
import System.Envy (runEnv, env)
import System.Process (readProcess)


getPhantomjsPath :: IO PhantomjsPath
getPhantomjsPath = do
  linkPath <- readProcess "which" ["phantomjs"] ""
  linkPath' <- readProcess "readlink" ["-f", linkPath] ""
  path <- trim <$> readProcess "readlink" ["-f", linkPath'] ""
  putStrLn $ "phantomjs path: " <> path
  return $ PhantomjsPath path


instance HasConfig IO where
  getConfig = config <$> getPhantomjsPath


main :: IO ()
main = do
  port <- either (const 8080) id <$> runEnv (env "TESTIFY_PORT")
  putStrLn $ "starting server on port " <> show port
  run port =<< app =<< getConfig
