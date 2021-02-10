{-# LANGUAGE DeriveGeneric #-}


module Types where


import Data.Aeson (ToJSON, ToJSONKey, FromJSON, FromJSONKey)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)


newtype Bill = Bill Text
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Bill
instance FromJSON Bill
instance ToJSONKey Bill
instance FromJSONKey Bill


newtype Committee = Committee Text
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Committee
instance FromJSON Committee
instance ToJSONKey Committee
instance FromJSONKey Committee


newtype Agenda = Agenda (Map Committee (Set Bill))
  deriving (Eq, Show, Read, Generic)

instance ToJSON Agenda
instance FromJSON Agenda


data Position = Support | Oppose | Neutral
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Position
instance FromJSON Position


newtype Positions = Positions (Map Committee (Map Bill Position))
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Positions
instance FromJSON Positions



