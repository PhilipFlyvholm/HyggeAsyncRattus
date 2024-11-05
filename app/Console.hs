{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Console where

import AsyncRattus.Channels (Producer, startEventLoop, setOutput)
import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import Behaviour (Behaviour (..), Fun (..), Time, Ô)
import Control.Concurrent (forkIO)
import qualified Data.IntSet as IntSet
import Data.Text hiding (filter, map)
import Data.Text.IO
import Data.Text.Read
import Event
import Prelude hiding (getLine, max, min)
import System.Exit

-- {-# ANN consoleInput AllowRecursion #-}

consoleInput :: IO (Box (Ô (Event Text)))
consoleInput = do
  (inp :* cb) <- getInputEvent
  let loop = do
        line <- getLine
        cb line
        loop
  forkIO loop
  return inp

setPrint :: (Producer p a, Show a) => p -> IO ()
setPrint event = setOutput event print

setQuit :: (Producer p a) => p -> IO ()
setQuit event = setOutput event (\_ -> exitSuccess)

startConsole = do
  console :: Ô (Event Text) <- unbox <$> consoleInput
  quitEvent :: Ô (Event ()) <- unbox <$> Event.filter (box (== "quit")) console

  setQuit quitEvent
  startEventLoop