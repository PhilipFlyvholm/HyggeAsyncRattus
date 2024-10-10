
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import SigMaybe
import Prelude hiding (init)
import Testing
import Behaviour
import System.Random (mkStdGen, StdGen)


zippedBeh :: Behaviour Int
zippedBeh = Behaviour.zipWith (box (+)) (beh 1) (beh' 1)

zippedOverlappingBeh :: Behaviour Int
zippedOverlappingBeh = Behaviour.zipWith (box (+)) (behA 1) (behB 1)

gen :: StdGen
gen = mkStdGen 137

main :: IO ()
main = do
  -- showNat <- trigger (box (\_ _ -> sample 5 zippedBeh gen)) everySecondSig (mMap (box (+ 1)) (nats 0))
  showNat <- trigger (box (\_ _ -> sample 5 zippedOverlappingBeh gen)) everySecondSig (mMap (box (+ 1)) (nats 0))
  setPrint showNat
  startEventLoop
