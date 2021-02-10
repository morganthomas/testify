{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Config where


import Data.String (IsString)
import Data.Text (Text)


newtype HouseFormUrl = HouseFormUrl Text
  deriving (Eq, Read, Show, IsString)


newtype SenateFormUrl = SenateFormUrl Text
  deriving (Eq, Read, Show, IsString)


data Config
  = Config
    { houseFormUrl :: HouseFormUrl
    , senateFormUrl :: SenateFormUrl
    }
  deriving (Eq, Read, Show)
