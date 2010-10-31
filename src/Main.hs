module Main where

import qualified Data.Map as Map

import Control.Monad.Trans (liftIO, MonadIO)

import qualified Network.MPD as MPD

import PlaybackState

printPlaybackState :: PlaybackState -> IO ()
printPlaybackState st = do
  putStrLn $ formatSong (currentSong st) ++ " " ++ (show $ elapsedTime st)
  where
    formatSong (Just s) = show $ Map.lookup MPD.Title $ MPD.sgTags s
    formatSong Nothing = "none"

main :: IO ()
main = do
  withMPD $ PlaybackState.onChange printPlaybackState

withMPD :: (MonadIO m) => MPD.MPD a -> m a
withMPD action = do
  result <- liftIO $ MPD.withMPD action
  case result of
      Left  e -> fail $ show e
      Right r -> return r
