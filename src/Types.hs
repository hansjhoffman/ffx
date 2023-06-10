module Types where

import RIO
import RIO.Process

newtype Options = Options
  { optionsVerbose :: Bool
  }

-- data Command
--   = Deploy TargetFile
--   | Init String
--   deriving (Eq, Show)
--
-- type TargetFile = String

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
