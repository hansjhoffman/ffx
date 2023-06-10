module Types where

import RIO
import RIO.Process

data Options = Options
  { optionsVerbose :: !Bool
  , optionsDebug :: !Bool
  }

data Command
  = Init Template
  | Publish TargetFile
  deriving (Eq, Show)

type TargetFile = String

data Template
  = TypeScript
  | JavaScript
  | Local String
  | Remote String
  deriving (Eq)

instance Show Template where
  show JavaScript = "javascript"
  show TypeScript = "typescript"
  show (Local filePath) = filePath
  show (Remote url) = url

data App = App
  { appFlatfileSecretKey :: !Text,
    appLogFn :: !LogFunc,
    appOptions :: !Options,
    appProcessContext :: !ProcessContext
  }

class HasFlatfileSecretKey env where
  flatfileSecretKeyL :: Lens' env Text

instance HasFlatfileSecretKey App where
  flatfileSecretKeyL = lens appFlatfileSecretKey (\x y -> x {appFlatfileSecretKey = y})

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x {appLogFn = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})