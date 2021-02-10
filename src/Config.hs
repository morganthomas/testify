{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Config where


import Data.String (IsString)
import Data.Text (Text)


newtype HouseFormUrl = HouseFormUrl { unHouseFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


newtype SenateFormUrl = SenateFormUrl { unSenateFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


data Config
  = Config
    { houseFormUrl :: HouseFormUrl
    , senateFormUrl :: SenateFormUrl
    }
  deriving (Eq, Read, Show)


config :: Config
config =
  Config
  { houseFormUrl = "http://gencourt.state.nh.us/house/committees/remotetestimony/default.aspx"
  , senateFormUrl = "http://gencourt.state.nh.us/remotecommittee/senate.aspx"
  }
