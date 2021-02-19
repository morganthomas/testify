{-# LANGUAGE OverloadedStrings #-}


module Main where


import Network.Wai.Handler.Warp (run)

import Api
import Automate
import Config
import Types
import Types.Api


main :: IO ()
main = run 8008 =<< app config
