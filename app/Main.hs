{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import AsyncRattus
import AsyncRattus.Channels
import Behaviour
import SigMaybe
import Strict (getCurrentStrictTime)
import System.Random (StdGen, mkStdGen)
import Testing
import Prelude hiding (init)
import Event

zippedBeh :: Behaviour Int
zippedBeh = Behaviour.zipWith (box (+)) (beh 1) (beh' 1)

zippedOverlappingBeh :: Behaviour Int
zippedOverlappingBeh = Behaviour.zipWith (box (+)) (behA 1) (behB 1)

gen :: StdGen
gen = mkStdGen 137

main :: IO ()
main = do
  -- showNat <- trigger (box (\_ _ -> sample 5 zippedBeh gen)) everySecondSig (mMap (box (+ 1)) (nats 0))
  -- showNat <- trigger (box (\_ _ -> sample 5 zippedOverlappingBeh gen)) everySecondSig (mMap (box (+ 1)) (nats 0))
  currentTimer <- Behaviour.startTimerBehaviour
  showNat <- Event.trigger (box (\_ _ -> sample 5 zippedOverlappingBeh gen)) everySecondEvent (Behaviour.map (box (+ 1)) currentTimer)
  currentTime <- getCurrentStrictTime
  print currentTime
  setPrint showNat
  startEventLoop
