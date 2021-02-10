{-# LANGUAGE OverloadedStrings #-}


module Automate where


import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day, toGregorian)
import Test.WebDriver.Class
import Test.WebDriver.Commands

import Config
import Types


getHouseBills :: WebDriver m => Config -> m Agenda
getHouseBills cfg = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  return (error "todo")


daySelector :: Day -> Text
daySelector day =
  let (_, month, dayOfMonth) = toGregorian day
      monthText = monthToText month
  in "a[title=\"" <> monthText <> " " <> pack (show dayOfMonth) <> "\"]"


monthToText :: Int -> Text
monthToText 1 = "January"
monthToText 2 = "February"
monthToText 3 = "March"
monthToText 4 = "April"
monthToText 5 = "May"
monthToText 6 = "June"
monthToText 7 = "July"
monthToText 8 = "August"
monthToText 9 = "September"
monthToText 10 = "October"
monthToText 11 = "November"
monthToText 12 = "December"
monthToText _ = "Septembruary"
