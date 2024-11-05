{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Console where

import AsyncRattus
import AsyncRattus.Channels
import Behaviour
import Control.Concurrent (forkIO)
import Control.Monad
import Data.Text hiding (filter, map)
import Data.Text.IO
import Event
import Prelude hiding (getLine, max, min, filter, map)
import System.Exit

{-# ANN consoleInput AllowRecursion #-}

consoleInput :: IO (Box (Ô (Event Text)))
consoleInput = do
         (inp :* cb) <- getInputEvent
         let loop = do line <- getLine
                       cb line
                       loop
         _ <- forkIO loop
         return inp

setPrint :: (Producer p a, Show a) => p -> IO ()
setPrint event = setOutput event print

setQuit :: (Producer p a) => p -> IO ()
setQuit event = setOutput event (const exitSuccess)

startConsole :: IO ()
startConsole = do
  console :: Ô (Event Text) <- unbox <$> consoleInput
  quitEvent :: Ô (Event Text) <- unbox <$> Event.filterAwait (box (== "quit")) console

  setQuit quitEvent
  startEventLoop