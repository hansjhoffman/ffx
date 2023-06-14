module Api.Id
  ( AccountId (..),
    AgentId (..),
    EnvironmentId (..),
  )
where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as J
import RIO
import RIO.Text qualified as T

newtype AccountId = AccountId
  { unAccountId :: Text
  }
  deriving (Eq)

instance Show AccountId where
  show (AccountId accountId) = "AccountId \"" <> T.unpack accountId <> "\""

instance FromJSON AccountId where
  parseJSON = J.withText "AccountId" (return . AccountId)

newtype AgentId = AgentId
  { unAgentId :: Text
  }
  deriving (Eq)

instance Show AgentId where
  show (AgentId agentId) = "AgentId \"" <> T.unpack agentId <> "\""

instance FromJSON AgentId where
  parseJSON = J.withText "AgentId" (return . AgentId)

newtype EnvironmentId = EnvironmentId
  { unEnvironmentId :: Text
  }
  deriving (Eq)

instance Show EnvironmentId where
  show (EnvironmentId envId) = "EnvironmentId \"" <> T.unpack envId <> "\""

instance FromJSON EnvironmentId where
  parseJSON = J.withText "EnvironmentId" (return . EnvironmentId)
