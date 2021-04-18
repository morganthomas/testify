{-# LANGUAGE OverloadedStrings #-}


module Main where


import Client                      (initialViewModel, initialPersons, getSavedPersonalInfo, UIM (runUIM), view)

import Control.Monad.IO.Class      (MonadIO (liftIO))
import Data.Maybe                  (fromMaybe)
import Data.Text                   (Text)
import Data.Time.Clock             (getCurrentTime, addUTCTime, nominalDay)
import Data.Time.LocalTime         (utcToLocalTime, getCurrentTimeZone, localDay)
import Shpadoinkle                 (shpadoinkle, JSM, newTVarIO)
import Shpadoinkle.Run             (runJSorWarp)
import Shpadoinkle.Backend.ParDiff (runParDiff)
import Shpadoinkle.DeveloperTools  (withDeveloperTools)
import Shpadoinkle.Html.Utils      (addStyle, getBody)


app :: JSM ()
app = do
  addStyle "https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/2.1.0/tailwind.min.css"
  now     <- liftIO getCurrentTime
  tz      <- liftIO getCurrentTimeZone
  persons <- fromMaybe initialPersons <$> getSavedPersonalInfo
  let tomorrow = localDay (utcToLocalTime tz (addUTCTime nominalDay now))
      initialModel = initialViewModel tomorrow persons
  model <- newTVarIO initialModel
  withDeveloperTools model
  shpadoinkle runUIM runParDiff model view getBody


main :: IO ()
main = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
