
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import SigMaybe
import Prelude hiding (init)
import Testing

main :: IO ()
main = do
  showNat <- trigger (box (\_ (Just' n) -> sample 5 (beh' n))) everySecondSig (mMap (box (+ 1)) (nats 0))
  setPrint showNat
  startEventLoop
