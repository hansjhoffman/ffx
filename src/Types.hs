module Types where

import Api.Id (EnvironmentId)
import RIO
import RIO.Process

data AppOptions = AppOptions
  { aoDebug :: !Bool,
    aoCommand :: !Command
  }
  deriving (Show)

data Command
  = Init Template
  | Publish FilePath
  deriving (Eq, Show)

data Template
  = JavaScript
  | Local FilePath
  | Remote String
  | TypeScript
  deriving (Eq)

instance Show Template where
  show JavaScript = "JavaScript"
  show (Local filePath) = filePath
  show (Remote url) = url
  show TypeScript = "TypeScript"

data App = App
  { appFlatfileEnvId :: !EnvironmentId,
    appFlatfileSecretKey :: !Text,
    appLogFn :: !LogFunc,
    appName :: !Text,
    appOptions :: !AppOptions,
    appProcessContext :: !ProcessContext
  }

class HasFlatfileEnvId env where
  flatfileEnvIdL :: Lens' env EnvironmentId

instance HasFlatfileEnvId App where
  flatfileEnvIdL = lens appFlatfileEnvId (\x y -> x {appFlatfileEnvId = y})

class HasFlatfileSecretKey env where
  flatfileSecretKeyL :: Lens' env Text

instance HasFlatfileSecretKey App where
  flatfileSecretKeyL = lens appFlatfileSecretKey (\x y -> x {appFlatfileSecretKey = y})

class HasName env where
  nameL :: Lens' env Text

instance HasName App where
  nameL = lens appName (\x y -> x {appName = y})

class HasAppOptions env where
  optsL :: Lens' env AppOptions

instance HasAppOptions App where
  optsL = lens appOptions (\x y -> x {appOptions = y})

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x {appLogFn = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
