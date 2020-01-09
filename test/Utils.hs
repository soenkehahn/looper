module Utils where

import Data.IORef
import Data.Traversable
import Foreign.C.Types
import Foreign.Marshal.Array
import Loopnaut.CBindings
import System.Timeout

withMockBindings :: (CBindings -> IO ()) -> IO [([CFloat], Int)]
withMockBindings action = do
  mockBuffer <- newIORef []
  let mockBindings = CBindings {
    create_loopnaut = do
      return (error "mock Ptr Buffer"),
    set_buffer = \ _ buffer len -> do
      modifyIORef mockBuffer (\ acc -> acc ++ [(buffer, len)])
  }
  action mockBindings
  setBuffers <- readIORef mockBuffer
  forM setBuffers $ \ (array, len) -> do
    peeked <- peekArray len array
    return (peeked, len)

timebox :: IO a -> IO ()
timebox action = do
  _ <- timeout 400000 action
  return ()
