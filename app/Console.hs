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

consoleInput :: IO (Box (OT (Event Text)))
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

getJustValues :: IO (OT (Event (Maybe' a))) -> IO (OT (Event a))
getJustValues e = do 
    e' <- e
    unbox <$> filterMapAwait (box id) e'

startConsole :: IO ()
startConsole = do
  console :: OT (Event Text) <- unbox <$> consoleInput
  quitEvent :: OT (Event Text) <- unbox <$> filterAwait (box (== "quit")) console
  showEvent :: OT (Event Text) <- unbox <$> filterAwait (box (== "show")) console
  resetEvent :: OT (Event Text) <- unbox <$> filterAwait (box (== "reset")) console

  startTimer :: Behaviour Int <- Behaviour.startTimerBehaviour
  lastReset :: Behaviour Int <- do
        n <- getJustValues (unbox <$> triggerAwaitIO (box (\_ n -> n)) resetEvent startTimer)
        let beh = mkBehaviourAwait n
        return (switch (K 0 :+: never) beh)
  let currentTimer :: Behaviour Int = Behaviour.zipWith (box (-)) startTimer lastReset
  

  showTimer :: OT (Event Int) <- getJustValues (unbox <$> triggerAwaitIO (box (\_ n -> n)) showEvent currentTimer)
 
  setPrint showTimer

  setQuit quitEvent
  startEventLoop