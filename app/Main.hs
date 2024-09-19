{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import Prelude hiding (init)

everySecond :: Box (O ()) -- Box makes the function stable and O() is a delayed unit
everySecond = timer 1000000 -- 1 second = 1.000.000 microseconds

everySecondSig :: Sig ()
everySecondSig =  () AsyncRattus.Signal.::: mkSig everySecond

setPrint :: (Producer p a, Show a) => p -> IO ()
setPrint sig = setOutput sig print

-- Scan http://www.zvon.org/other/haskell/Outputprelude/scanl_f.html
nats :: Int -> Sig Int
nats init = scan (box (\ n _ -> n+1)) init everySecondSig


newtype SigMaybe a = SigMaybe (Sig (Maybe' a))
 
unSigMaybe :: SigMaybe a -> Sig (Maybe' a)
unSigMaybe (SigMaybe x) = x

mMap :: Box (a -> b) -> SigMaybe a -> SigMaybe b
mMap f (SigMaybe (s ::: xs)) = 
    case s of
        Nothing' -> SigMaybe (Nothing' ::: delay (unSigMaybe (mMap f (SigMaybe (adv xs)))))
        Just' a ->  SigMaybe (Just' (unbox f a) ::: delay (unSigMaybe (mMap f (SigMaybe (adv xs)))))

mScan :: Stable b => Box (b -> a -> b) -> b -> SigMaybe a -> SigMaybe b
mScan f acc (SigMaybe (x ::: xs)) =
    case x of 
        Nothing' -> SigMaybe (Nothing' ::: delay (unSigMaybe (mScan f acc (SigMaybe (adv xs)))))
        Just' a  -> SigMaybe (Just' acc' ::: delay (unSigMaybe (mScan f acc (SigMaybe (adv xs)))))
            where acc' = unbox f acc a

main :: IO ()
main = do
    showNat <- trigger (box (\_ n -> n)) everySecondSig (nats 0)

    setPrint showNat
    startEventLoop

