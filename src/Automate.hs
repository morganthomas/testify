module Automate where


import Data.Text (unpack)
import Test.WebDriver.Class
import Test.WebDriver.Commands

import Config
import Types


getHouseBills :: WebDriver m => Config -> m Agenda
getHouseBills cfg = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  return (error "todo")
