{-# LANGUAGE DeriveGeneric #-}


module Types where


import Data.Aeson (ToJSON, ToJSONKey, FromJSON, FromJSONKey)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)


newtype BillName = BillName { unBillName :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON BillName
instance FromJSON BillName


newtype BillId = BillId { unBillId :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON BillId
instance FromJSON BillId


data Bill
  = Bill
    { billName :: BillName
    , billId   :: BillId }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Bill
instance FromJSON Bill
instance ToJSONKey Bill
instance FromJSONKey Bill


newtype CommitteeName = CommitteeName { unCommitteeName :: Text }
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON CommitteeName
instance FromJSON CommitteeName


newtype CommitteeId = CommitteeId { unCommitteeId :: Text }
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON CommitteeId
instance FromJSON CommitteeId


data Committee =
  Committee
  { committeeName :: CommitteeName
  , committeeId   :: CommitteeId
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Committee
instance FromJSON Committee
instance ToJSONKey Committee
instance FromJSONKey Committee


newtype Agenda = Agenda { unAgenda :: Map Committee (Set Bill) }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Agenda
instance FromJSON Agenda


data Position = Support | Oppose | Neutral
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Position
instance FromJSON Position


newtype Positions = Positions { unPositions :: Map Committee (Map Bill Position) }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Positions
instance FromJSON Positions


newtype FirstName = FirstName { unFirstName :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON FirstName
instance FromJSON FirstName


newtype LastName = LastName { unLastName :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON LastName
instance FromJSON LastName


newtype Email = Email { unEmail :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Email
instance FromJSON Email


newtype Town = Town { unTown :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

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

instance ToJSON PersonalInfo
instance FromJSON PersonalInfo


data Submission
  = Submission
    { positions      :: Positions
    , persons        :: [PersonalInfo]
    , submissionDate :: Day
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Submission
instance FromJSON Submission


newtype ErrorMessage = ErrorMessage Text
  deriving (Eq, Show, Read, Generic)

instance ToJSON ErrorMessage
instance FromJSON ErrorMessage


newtype AgendaResult = AgendaResult { unAgendaResult :: Either ErrorMessage Agenda }
  deriving (Eq, Show, Read, Generic)

instance ToJSON AgendaResult
instance FromJSON AgendaResult


data Success = Success
  deriving (Eq, Show, Read, Generic)

instance ToJSON Success
instance FromJSON Success


newtype TestifyResult = TestifyResult { unTestifyResult :: Either ErrorMessage Success }
  deriving (Eq, Show, Read, Generic)

instance ToJSON TestifyResult
instance FromJSON TestifyResult
