{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}

module ZippedWithTest where

import AsyncRattus
import AsyncRattus.Channels
import Behaviour
import Console
import Event
import Strict (getCurrentStrictTime)
import System.Random (StdGen, mkStdGen)
import Testing
import Prelude hiding (init)

zippedBeh :: Behaviour Int
zippedBeh = Behaviour.zipWith (box (+)) (beh 1) (beh' 1)

zippedOverlappingBeh :: Behaviour Int
zippedOverlappingBeh = Behaviour.zipWith (box (+)) (behA 1) (behB 1)

gen :: StdGen
gen = mkStdGen 137

zippedWithTest :: IO ()
zippedWithTest = do
  -- showNat <- trigger (box (\_ _ -> sample 5 zippedBeh gen)) everySecondSig (mMap (box (+ 1)) (nats 0))
  -- showNat <- trigger (box (\_ _ -> sample 5 zippedOverlappingBeh gen)) everySecondSig (mMap (box (+ 1)) (nats 0))
  currentTimer <- Behaviour.startTimerBehaviour

  showZippedBehavioursEverySecond <- Event.trigger (box (\_ _ -> sample 5 zippedOverlappingBeh gen)) everySecondEvent (Behaviour.map (box (+ 1)) currentTimer)

  currentTime <- getCurrentStrictTime
  print currentTime
  setPrint showZippedBehavioursEverySecond
  startEventLoop
