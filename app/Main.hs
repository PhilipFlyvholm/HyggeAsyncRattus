{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import Prelude hiding (init)
import SigMaybe
import Data.Time

type Time = Data.Time.UTCTime

type Ô a = O(a, Time)

data Fun t a = K a | Fun (t -> a)

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = f

data Behaviour a = (Fun Time a) :+: Ô(Behaviour a)

main :: IO ()
main = do
  showNat <- trigger (box (\_ n -> n)) everySecondSig (mMap (box (+ 1)) (nats 0))

  setPrint showNat
  startEventLoop
