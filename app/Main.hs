{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import Prelude hiding (init)

everySecond :: Box (O ()) -- Box makes the function stable and O() is a delayed unit
everySecond = timer 1000000 -- 1 second = 1.000.000 microseconds

everySecondSig :: Sig ()
everySecondSig = () AsyncRattus.Signal.::: mkSig everySecond

setPrint :: (Producer p a, Show a) => p -> IO ()
setPrint sig = setOutput sig print

-- Scan http://www.zvon.org/other/haskell/Outputprelude/scanl_f.html
nats :: Int -> SigMaybe Int
nats init = scan (box (\_ _ -> Just' 1)) (Just' init) everySecondSig

type SigMaybe a = (Sig (Maybe' a))

mMap :: Box (a -> b) -> SigMaybe a -> SigMaybe b
mMap f =
  AsyncRattus.Signal.map
    ( box
        ( \case
            Just' x -> Just' (unbox f x)
            Nothing' -> Nothing'
        )
    )

mScan :: (Stable b) => Box (b -> a -> b) -> b -> SigMaybe a -> SigMaybe b
mScan f acc (x ::: xs) =
  case x of
    Nothing' -> Nothing' ::: delay (mScan f acc (adv xs))
    Just' a -> Just' acc' ::: delay (mScan f acc (adv xs))
      where
        acc' = unbox f acc a

mFilter :: Box (a -> Bool) -> SigMaybe a -> SigMaybe a
mFilter f (s ::: xs) =
  case s of
    Just' a -> (if unbox f a then Just' a else Nothing') ::: delay (mFilter f (adv xs))
    Nothing' -> Nothing' ::: delay (mFilter f (adv xs))

main :: IO ()
main = do
  showNat <- trigger (box (\_ n -> n)) everySecondSig (mMap (box (+ 1)) (nats 0))

  setPrint showNat
  startEventLoop
