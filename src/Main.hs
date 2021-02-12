{-# LANGUAGE OverloadedStrings #-}


module Main where


import Network.Wai.Handler.Warp (run)
import           Shpadoinkle                 (Html, JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             (live, runJSorWarp, simple)

import Api
import Automate
import Config
import Types
import Types.Api


{-
view :: () -> Html m ()
view _ = "hello world"


app :: JSM ()
app = simple runParDiff () view getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
-}

main :: IO ()
main = run 8008 =<< app config
