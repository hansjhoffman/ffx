module Api.Agent
  ( Agent (..),
    create,
  )
where

import Api.Id (AgentId)
import Api.Id qualified as Id
import Data.Aeson
  ( FromJSON (..),
    ToJSON,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Version qualified as V
import Network.HTTP.Simple
  ( JSONException,
    Request,
  )
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Header as Header
import Paths_ffx qualified as Meta
import RIO
import RIO.Text qualified as T
import Types

data Agent = Agent
  { agentId :: !AgentId,
    agentTopics :: !(Maybe [EventTopic]),
    agentComplier :: !(Maybe Text),
    agentSource :: !(Maybe Text)
  }
  deriving (Eq, Show)

instance FromJSON Agent where
  parseJSON = J.withObject "Agent" $ \o -> do
    obj <- o .: "data"
    Agent
      <$> (obj .: "id")
      <*> (obj .:? "topics")
      <*> (obj .:? "compiler")
      <*> (obj .:? "source")

data EventTopic
  = AgentCreated
  | AgentDeleted
  | AgentUpdated
  | CommitCreated
  | CommitUpdated
  | DocumentCreated
  | DocumentDeleted
  | DocumentUpdated
  | FileCreated
  | FileDeleted
  | FileUpdated
  | JobAcknowledged
  | JobCompleted
  | JobCreated
  | JobDeleted
  | JobFailed
  | JobReady
  | JobScheduled
  | JobUpdated
  | LayerCreated
  | RecordCreated
  | RecordDeleted
  | RecordUpdated
  | SheetCreated
  | SheetDeleted
  | SheetUpdated
  | SpaceCreated
  | SpaceDeleted
  | SpaceUpdated
  | WorkbookCreated
  | WorkbookDeleted
  | WorkbookUpdated
  deriving (Eq, Generic)

instance Show EventTopic where
  show AgentCreated = "agent:created"
  show AgentDeleted = "agent:deleted"
  show AgentUpdated = "agent:updated"
  show CommitCreated = "commit:created"
  show CommitUpdated = "commit:updated"
  show DocumentCreated = "document:created"
  show DocumentDeleted = "document:deleted"
  show DocumentUpdated = "document:updated"
  show FileCreated = "file:created"
  show FileDeleted = "file:deleted"
  show FileUpdated = "file:updated"
  show JobAcknowledged = "job:outcome-acknowledged"
  show JobCompleted = "job:completed"
  show JobCreated = "job:created"
  show JobDeleted = "job:deleted"
  show JobFailed = "job:failed"
  show JobReady = "job:ready"
  show JobScheduled = "job:scheduled"
  show JobUpdated = "job:updated"
  show LayerCreated = "layer:created"
  show RecordCreated = "record:created"
  show RecordDeleted = "record:deleted"
  show RecordUpdated = "record:updated"
  show SheetCreated = "sheet:created"
  show SheetDeleted = "sheet:deleted"
  show SheetUpdated = "sheet:updated"
  show SpaceCreated = "space:created"
  show SpaceDeleted = "space:deleted"
  show SpaceUpdated = "space:updated"
  show WorkbookCreated = "workbook:created"
  show WorkbookDeleted = "workbook:deleted"
  show WorkbookUpdated = "workbook:updated"

instance FromJSON EventTopic where
  parseJSON = J.withText "EventTopic" $ \case
    "agent:created" -> pure AgentCreated
    "agent:deleted" -> pure AgentDeleted
    "agent:updated" -> pure AgentUpdated
    "commit:created" -> pure CommitCreated
    "commit:updated" -> pure CommitUpdated
    "document:created" -> pure DocumentCreated
    "document:deleted" -> pure DocumentDeleted
    "document:updated" -> pure DocumentUpdated
    "file:created" -> pure FileCreated
    "file:deleted" -> pure FileDeleted
    "file:updated" -> pure FileUpdated
    "job:completed" -> pure JobCompleted
    "job:created" -> pure JobCreated
    "job:deleted" -> pure JobDeleted
    "job:failed" -> pure JobFailed
    "job:outcome-acknowledged" -> pure JobAcknowledged
    "job:ready" -> pure JobReady
    "job:scheduled" -> pure JobScheduled
    "job:updated" -> pure JobUpdated
    "layer:created" -> pure LayerCreated
    "record:created" -> pure RecordCreated
    "record:deleted" -> pure RecordDeleted
    "record:updated" -> pure RecordUpdated
    "sheet:created" -> pure SheetCreated
    "sheet:deleted" -> pure SheetDeleted
    "sheet:updated" -> pure SheetUpdated
    "space:created" -> pure SpaceCreated
    "space:deleted" -> pure SpaceDeleted
    "space:updated" -> pure SpaceUpdated
    "workbook:created" -> pure WorkbookCreated
    "workbook:deleted" -> pure WorkbookDeleted
    "workbook:updated" -> pure WorkbookUpdated
    _ -> fail "Unknown event topic"

instance ToJSON EventTopic where
  toJSON = \case
    AgentCreated -> "agent:created"
    AgentDeleted -> "agent:deleted"
    AgentUpdated -> "agent:updated"
    CommitCreated -> "commit:created"
    CommitUpdated -> "commit:updated"
    DocumentCreated -> "document:created"
    DocumentDeleted -> "document:deleted"
    DocumentUpdated -> "document:updated"
    FileCreated -> "file:created"
    FileDeleted -> "file:deleted"
    FileUpdated -> "file:updated"
    JobAcknowledged -> "job:outcome-acknowledged"
    JobCompleted -> "job:completed"
    JobCreated -> "job:created"
    JobDeleted -> "job:deleted"
    JobFailed -> "job:failed"
    JobReady -> "job:ready"
    JobScheduled -> "job:scheduled"
    JobUpdated -> "job:updated"
    LayerCreated -> "layer:created"
    RecordCreated -> "record:created"
    RecordDeleted -> "record:deleted"
    RecordUpdated -> "record:updated"
    SheetCreated -> "sheet:created"
    SheetDeleted -> "sheet:deleted"
    SheetUpdated -> "sheet:updated"
    SpaceCreated -> "space:created"
    SpaceDeleted -> "space:deleted"
    SpaceUpdated -> "space:updated"
    WorkbookCreated -> "workbook:created"
    WorkbookDeleted -> "workbook:deleted"
    WorkbookUpdated -> "workbook:updated"

  toEncoding =
    JE.text . \case
      AgentCreated -> "agent:created"
      AgentDeleted -> "agent:deleted"
      AgentUpdated -> "agent:updated"
      CommitCreated -> "commit:created"
      CommitUpdated -> "commit:updated"
      DocumentCreated -> "document:created"
      DocumentDeleted -> "document:deleted"
      DocumentUpdated -> "document:updated"
      FileCreated -> "file:created"
      FileDeleted -> "file:deleted"
      FileUpdated -> "file:updated"
      JobAcknowledged -> "job:outcome-acknowledged"
      JobCompleted -> "job:completed"
      JobCreated -> "job:created"
      JobDeleted -> "job:deleted"
      JobFailed -> "job:failed"
      JobReady -> "job:ready"
      JobScheduled -> "job:scheduled"
      JobUpdated -> "job:updated"
      LayerCreated -> "layer:created"
      RecordCreated -> "record:created"
      RecordDeleted -> "record:deleted"
      RecordUpdated -> "record:updated"
      SheetCreated -> "sheet:created"
      SheetDeleted -> "sheet:deleted"
      SheetUpdated -> "sheet:updated"
      SpaceCreated -> "space:created"
      SpaceDeleted -> "space:deleted"
      SpaceUpdated -> "space:updated"
      WorkbookCreated -> "workbook:created"
      WorkbookDeleted -> "workbook:deleted"
      WorkbookUpdated -> "workbook:updated"

buildRequest :: App -> Text -> Request
buildRequest env sourceCode =
  let host :: ByteString
      host = T.encodeUtf8 "platform.flatfile.com"

      path :: ByteString
      path = T.encodeUtf8 "api/v1/agents"

      flatfileSecretKey :: ByteString
      flatfileSecretKey = T.encodeUtf8 $ view flatfileSecretKeyL env

      envId :: ByteString
      envId = T.encodeUtf8 $ Id.unEnvironmentId (view flatfileEnvIdL env)

      versionString :: ByteString
      versionString = T.encodeUtf8 $ view nameL env <> " v" <> T.pack (V.showVersion Meta.version)

      jsonBody :: J.Value
      jsonBody =
        J.object
          [ "topics" .= [RecordCreated],
            "compiler" .= ("js" :: String),
            "source" .= sourceCode
          ]
   in HTTP.setRequestHost host
        $ HTTP.setRequestMethod "POST"
        $ HTTP.setRequestPort 443
        $ HTTP.setRequestSecure True
        $ HTTP.setRequestPath path
        $ HTTP.setRequestQueryString
          [ ("environmentId", Just envId)
          ]
        $ HTTP.setRequestHeaders
          [ (Header.hAuthorization, "Bearer " <> flatfileSecretKey),
            (Header.hContentType, T.encodeUtf8 "application/json"),
            (Header.hUserAgent, versionString)
          ]
        $ HTTP.setRequestBodyJSON
          jsonBody
          HTTP.defaultRequest

create :: Text -> RIO App (Either JSONException Agent)
create sourceCode = do
  env <- ask
  response <- HTTP.httpJSONEither $ buildRequest env sourceCode
  logDebug $ displayShow response
  return $ HTTP.getResponseBody response
