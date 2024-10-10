
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import SigMaybe
import Prelude hiding (init)
import Testing
import Behaviour

zippedBeh :: Behaviour Int
zippedBeh = Behaviour.zipWith (box (+)) (beh 1) (beh' 2)

main :: IO ()
main = do
  showNat <- trigger (box (\_ _ -> sample 5 zippedBeh)) everySecondSig (mMap (box (+ 1)) (nats 0))
  setPrint showNat
  startEventLoop
