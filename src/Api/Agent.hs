module Api.Agent
  ( Agent,
    create,
  )
where

import Api.Id (AgentId)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as JSON
import RIO

data Agent = Agent
  { agentId :: !AgentId,
    agentTopics :: ![Text],
    agentComplier :: !Text,
    agentSource :: !Text
  }
  deriving (Eq, Show)

instance FromJSON Agent where
  parseJSON = JSON.withObject "Agent" $ \obj ->
    Agent
      <$> (obj .: "id")
      <*> (obj .: "topics")
      <*> (obj .: "compiler")
      <*> (obj .: "source")

create :: Agent -> IO Agent
create = undefined
