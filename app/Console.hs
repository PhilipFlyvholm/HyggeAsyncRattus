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
  quitEvent :: Ô (Event Text) <- unbox <$> filterAwait (box (== "quit")) console
  showEvent :: Ô (Event Text) <- unbox <$> filterAwait (box (== "show")) console
  --resetEvent :: Ô (Event Text) <- unbox <$> filterAwait (box (== "reset")) console

  currentTimer :: Behaviour Int <- Behaviour.startTimerBehaviour

  showTimerRaw :: Ô (Event (Maybe' Int)) <- unbox <$> triggerAwaitIO (box (\_ n -> n)) showEvent currentTimer
  showTimer :: Ô (Event Int) <- unbox <$> filterMapAwait (box id) showTimerRaw

  setPrint showTimer

  setQuit quitEvent
  startEventLoop