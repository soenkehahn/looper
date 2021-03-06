module Looper.FileWatcher (fileWatcher) where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Word
import Looper.FileWatcher.Common
import System.OSX.FSEvents

fileWatcher :: FileWatcher
fileWatcher = FileWatcher {
  watchFiles = \ watchedFiles handle action -> do
    if (length watchedFiles >= 1) then do
      wrapCallback <- callbackExceptionPropagator
      let start = eventStreamCreate watchedFiles 0.2 False False True $ \ event -> wrapCallback $ do
            let isTriggeringEvent =
                  not (any (flagIsSet event) [EventFlagItemRemoved, EventFlagItemRenamed]) &&
                  flagIsSet event EventFlagItemModified
            when isTriggeringEvent $ do
              handleWhenWatched watchedFiles (eventPath event) handle
      bracket start eventStreamDestroy (const action)
    else do
      action
}

data Flag
  = EventFlagMustScanSubDirs
  | EventFlagUserDropped
  | EventFlagKernelDropped
  | EventFlagEventIdsWrapped
  | EventFlagHistoryDone
  | EventFlagRootChanged
  | EventFlagMount
  | EventFlagUnmount
  | EventFlagItemCreated
  | EventFlagItemRemoved
  | EventFlagItemInodeMetaMod
  | EventFlagItemRenamed
  | EventFlagItemModified
  | EventFlagItemFinderInfoMod
  | EventFlagItemChangeOwner
  | EventFlagItemXattrMod
  | EventFlagItemIsFile
  | EventFlagItemIsDir
  | EventFlagItemIsSymlink
  deriving (Eq, Show, Enum, Bounded)

flagIsSet :: Event -> Flag -> Bool
flagIsSet event flag = (eventFlags event .&. toWord flag) /= 0

toWord :: Flag -> Word64
toWord flag = case flag of
  EventFlagMustScanSubDirs ->  eventFlagMustScanSubDirs
  EventFlagUserDropped ->  eventFlagUserDropped
  EventFlagKernelDropped ->  eventFlagKernelDropped
  EventFlagEventIdsWrapped ->  eventFlagEventIdsWrapped
  EventFlagHistoryDone ->  eventFlagHistoryDone
  EventFlagRootChanged ->  eventFlagRootChanged
  EventFlagMount ->  eventFlagMount
  EventFlagUnmount ->  eventFlagUnmount
  EventFlagItemCreated ->  eventFlagItemCreated
  EventFlagItemRemoved ->  eventFlagItemRemoved
  EventFlagItemInodeMetaMod ->  eventFlagItemInodeMetaMod
  EventFlagItemRenamed ->  eventFlagItemRenamed
  EventFlagItemModified ->  eventFlagItemModified
  EventFlagItemFinderInfoMod ->  eventFlagItemFinderInfoMod
  EventFlagItemChangeOwner ->  eventFlagItemChangeOwner
  EventFlagItemXattrMod ->  eventFlagItemXattrMod
  EventFlagItemIsFile ->  eventFlagItemIsFile
  EventFlagItemIsDir ->  eventFlagItemIsDir
  EventFlagItemIsSymlink ->  eventFlagItemIsSymlink

-- useful for debugging
_toFlags :: Event -> [Flag]
_toFlags event =
  filter (\ flag -> flagIsSet event flag) [minBound .. maxBound]
