{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}


module Primitives where

import AsyncRattus
import StrictUTCTime (UTCTime')

type Time = UTCTime'

type OT a = O (a :* Time)

data Fun t a = K !a | Fun !(Box (t -> a))

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

applyF :: Box (a -> b) -> Fun t a -> Fun t b
applyF f (K a) = K (unbox f a)
applyF f (Fun t) = Fun (box (unbox f . unbox t))