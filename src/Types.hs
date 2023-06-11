module Types where

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
  = TypeScript
  | JavaScript
  | Local FilePath
  | Remote String
  deriving (Eq)

instance Show Template where
  show JavaScript = "JavaScript"
  show TypeScript = "TypeScript"
  show (Local filePath) = filePath
  show (Remote url) = url

data App = App
  { appFlatfileEnvId :: !Text,
    appFlatfileSecretKey :: !Text,
    appLogFn :: !LogFunc,
    appOptions :: !AppOptions,
    appProcessContext :: !ProcessContext
  }

class HasFlatfileEnvId env where
  flatfileEnvIdL :: Lens' env Text

instance HasFlatfileEnvId App where
  flatfileEnvIdL = lens appFlatfileEnvId (\x y -> x {appFlatfileEnvId = y})

class HasFlatfileSecretKey env where
  flatfileSecretKeyL :: Lens' env Text

instance HasFlatfileSecretKey App where
  flatfileSecretKeyL = lens appFlatfileSecretKey (\x y -> x {appFlatfileSecretKey = y})

class HasAppOptions env where
  optsL :: Lens' env AppOptions

instance HasAppOptions App where
  optsL = lens appOptions (\x y -> x {appOptions = y})

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x {appLogFn = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
