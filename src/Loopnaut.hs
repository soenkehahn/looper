{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad
import SFML.Audio.Music
import SFML.Audio.Types
import System.IO
import WithCli

main :: IO ()
main = withCli $ \ files -> do
  case files of
    [] ->
      hPutStrLn stderr "no files given"
    _ -> do
      tracks <- mapM loadTrack files
      foo tracks
      forever $ threadDelay 1000000

loadTrack :: FilePath -> IO Music
loadTrack file = do
  result <- musicFromFile file
  case result of
    Right music -> return music

foo :: [Music] -> IO ()
foo tracks = do
  forM_ tracks $ \ track ->
    setLoop track True
  forM_ tracks $ \ track ->
    play track
