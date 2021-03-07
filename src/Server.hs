{-# LANGUAGE OverloadedStrings #-}


module Main where


import Network.Wai.Handler.Warp (run)

import Api
import Automate
import Config
import System.Process (readProcess)
import Types
import Types.Api


getPhantomjsPath :: IO PhantomjsPath
getPhantomjsPath = PhantomjsPath <$> readProcess "which" ["phantomjs"] ""


instance HasConfig IO where
  getConfig = config <$> getPhantomjsPath


main :: IO ()
main = run 8008 =<< app =<< getConfig
