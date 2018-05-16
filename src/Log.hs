{-# OPTIONS_GHC -fno-warn-tabs #-}

module Log where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.LocalTime (getZonedTime)

logFileName = "log.txt"

initLog :: (MonadIO m) => m ()
initLog = liftIO $ do
	now <- getZonedTime
	writeFile logFileName $ "Log start: " ++ show now ++ "\n#######################################\n\n"

logMsg :: (MonadIO m) => String -> m ()
logMsg msg = liftIO $ appendFile logFileName msg
