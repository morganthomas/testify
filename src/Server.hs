{-# LANGUAGE OverloadedStrings #-}


module Main where


import Network.Wai.Handler.Warp (run)

import Api
import Automate
import Config
import Types
import Types.Api

import System.Process (readProcess)


getPhantomjsPath :: IO PhantomjsPath
getPhantomjsPath = PhantomjsPath <$> readProcess "which" ["phantomjs"] ""


instance HasConfig IO where
  getConfig = config <$> getPhantomjsPath


main :: IO ()
main = run 8008 =<< app =<< getConfig
