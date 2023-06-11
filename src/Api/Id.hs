module Api.Id
  ( AccountId (..),
    AgentId (..),
    EnvironmentId (..),
  )
where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as JSON
import RIO

newtype AccountId = AccountId
  { unAccountId :: Text
  }
  deriving (Eq, Show)

instance FromJSON AccountId where
  parseJSON = JSON.withText "AccountId" (return . AccountId)

newtype AgentId = AgentId
  { unAgentId :: Text
  }
  deriving (Eq, Show)

instance FromJSON AgentId where
  parseJSON = JSON.withText "AgentId" (return . AgentId)

newtype EnvironmentId = EnvironmentId
  { unEnvironmentId :: Text
  }
  deriving (Eq, Show)

instance FromJSON EnvironmentId where
  parseJSON = JSON.withText "EnvironmentId" (return . EnvironmentId)
