{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}


module Types where


import Data.Aeson (ToJSON, ToJSONKey, FromJSON, FromJSONKey)
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Shpadoinkle (NFData)


data Chamber = House | Senate
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Chamber
instance ToJSON Chamber
instance FromJSON Chamber


data MultiPersonFeature = MultiPersonFeature | NoMultiPersonFeature
  deriving (Eq, Ord, Show, Read, Generic)


data Features =
  Features
  { multiPersonFeature :: MultiPersonFeature
  } deriving (Eq, Ord, Show, Read, Generic)


newtype BillName = BillName { unBillName :: Text }
  deriving (Eq, Ord, Show, Read, Generic, IsString)

instance NFData BillName
instance ToJSON BillName
instance FromJSON BillName


newtype BillId = BillId { unBillId :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData BillId
instance ToJSON BillId
instance FromJSON BillId


data Bill
  = Bill
    { billName :: BillName
    , billId   :: BillId }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Bill
instance ToJSON Bill
instance FromJSON Bill
instance ToJSONKey Bill
instance FromJSONKey Bill


newtype CommitteeName = CommitteeName { unCommitteeName :: Text }
  deriving (Eq, Ord, Read, Show, Generic, IsString)

instance NFData CommitteeName
instance ToJSON CommitteeName
instance FromJSON CommitteeName


newtype CommitteeId = CommitteeId { unCommitteeId :: Text }
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData CommitteeId
instance ToJSON CommitteeId
instance FromJSON CommitteeId


data Committee =
  Committee
  { committeeName :: CommitteeName
  , committeeId   :: CommitteeId
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Committee
instance ToJSON Committee
instance FromJSON Committee
instance ToJSONKey Committee
instance FromJSONKey Committee


newtype Agenda = Agenda { unAgenda :: Map Committee (Set Bill) }
  deriving (Eq, Show, Read, Generic)

instance NFData Agenda
instance ToJSON Agenda
instance FromJSON Agenda


data Position = Support | Oppose | Neutral
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Position
instance ToJSON Position
instance FromJSON Position


newtype Positions = Positions { unPositions :: Map Committee (Map Bill (Maybe Position)) }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Positions
instance ToJSON Positions
instance FromJSON Positions


newtype FirstName = FirstName { unFirstName :: Text }
  deriving (Eq, Ord, Show, Read, Generic, IsString)

instance NFData FirstName
instance ToJSON FirstName
instance FromJSON FirstName


newtype LastName = LastName { unLastName :: Text }
  deriving (Eq, Ord, Show, Read, Generic, IsString)

instance NFData LastName
instance ToJSON LastName
instance FromJSON LastName


newtype Email = Email { unEmail :: Text }
  deriving (Eq, Ord, Show, Read, Generic, IsString)

instance NFData Email
instance ToJSON Email
instance FromJSON Email


newtype Town = Town { unTown :: Text }
  deriving (Eq, Ord, Show, Read, Generic, IsString)

instance NFData Town
instance ToJSON Town
instance FromJSON Town


data PersonalInfo
  = PersonalInfo
    { firstName :: FirstName
    , lastName  :: LastName
    , email     :: Email
    , town      :: Town
    }
  deriving (Eq, Show, Read, Generic)

instance NFData PersonalInfo
instance ToJSON PersonalInfo
instance FromJSON PersonalInfo


data Submission
  = Submission
    { positions      :: Positions
    , persons        :: [PersonalInfo]
    , submissionDate :: Day
    }
  deriving (Eq, Show, Read, Generic)

instance NFData Submission
instance ToJSON Submission
instance FromJSON Submission


newtype ErrorMessage = ErrorMessage { unErrorMessage :: Text }
  deriving (Eq, Show, Read, Generic)

instance NFData ErrorMessage
instance ToJSON ErrorMessage
instance FromJSON ErrorMessage


newtype AgendaResult = AgendaResult { unAgendaResult :: Either ErrorMessage Agenda }
  deriving (Eq, Show, Read, Generic)

instance NFData AgendaResult
instance ToJSON AgendaResult
instance FromJSON AgendaResult


data Success = Success
  deriving (Eq, Show, Read, Generic)

instance NFData Success
instance ToJSON Success
instance FromJSON Success


newtype TestifyResult = TestifyResult { unTestifyResult :: Either ErrorMessage Success }
  deriving (Eq, Show, Read, Generic)

instance NFData TestifyResult
instance ToJSON TestifyResult
instance FromJSON TestifyResult
