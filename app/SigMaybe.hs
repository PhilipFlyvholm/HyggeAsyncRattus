{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module SigMaybe where

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
mScan f acc (Just' a ::: xs) = Just' (unbox f acc a) ::: delay (mScan f acc (adv xs))
mScan f acc (Nothing' ::: xs) = Nothing' ::: delay (mScan f acc (adv xs))

mFilter :: Box (a -> Bool) -> SigMaybe a -> SigMaybe a
mFilter f (Just' a ::: xs) = (if unbox f a then Just' a else Nothing') ::: delay (mFilter f (adv xs))
mFilter f (Nothing' ::: xs) = Nothing' ::: delay (mFilter f (adv xs))

{- mInterleave :: Box ([a] -> a) -> [O (Sig a)] -> O(Sig a)
mInterleave f [s] = s
mInterleave f (x:xs) = interleave (f x (mInterleave f xs)) -}

