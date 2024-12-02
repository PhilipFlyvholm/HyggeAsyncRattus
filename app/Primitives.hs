{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}


module Primitives where

import AsyncRattus
import StrictUTCTime (UTCTime', getCurrentStrictTime)
import AsyncRattus.Channels (Producer, getInput, setOutput)

type Time = UTCTime'

type OT a = O (a :* Time)

mapOT :: Box (a -> b) -> OT a -> OT b
mapOT f later = delay (let (v :* t) = adv later in (unbox f v :* t))

data Fun t a = K !a | Fun !(Box (t -> a))

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

applyF :: Box (a -> b) -> Fun t a -> Fun t b
applyF f (K a) = K (unbox f a)
applyF f (Fun t) = Fun (box (unbox f . unbox t))

getInputWithTime :: IO (Box (OT a) :* (a -> IO ()))
getInputWithTime = do
  (b :* f) <- getInput
  return
    ( b :* \a -> do
        t <- getCurrentStrictTime
        f (a :* t)
    )
    
mkInputWithTime :: (Producer p a) => p -> IO (Box (OT a))
mkInputWithTime p = do
  (out :* cb) <- getInputWithTime
  setOutput p cb
  return out