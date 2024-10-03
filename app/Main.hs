{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import SigMaybe
import Prelude hiding (init)

type Time = Integer

type Ô a = O (a, Time)

data Fun t a = K a | Fun (t -> a)

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = f

data Behaviour a = (Fun Time a) :+: Ô (Behaviour a)

-- adv' :: Ô a -> InputValue -> a
-- adv' (Delay f) inp = f inp

mapB :: Box (a -> b) -> Behaviour a -> Behaviour b
mapB f (K a :+: xs) = K (unbox f a) :+: delay (let (b', t) = adv xs in (mapB f b', t))
mapB f (Fun t :+: xs) = Fun (\t' -> unbox f (t t')) :+: delay (let (b', t') = adv xs in (mapB f b', t'))

-- mFilter :: Box (a -> Bool) -> SigMaybe a -> SigMaybe a
-- mFilter f (Just' a ::: xs) = (if unbox f a then Just' a else Nothing') ::: delay (mFilter f (adv xs))
-- mFilter f (Nothing' ::: xs) = Nothing' ::: delay (mFilter f (adv xs))

filterB :: Box (a -> Bool) -> Behaviour a -> Behaviour (Maybe' a)
filterB f = mapB (box (\x -> if unbox f x then Just' x else Nothing'))

main :: IO ()
main = do
  showNat <- trigger (box (\_ n -> n)) everySecondSig (mMap (box (+ 1)) (nats 0))

  setPrint showNat
  startEventLoop
