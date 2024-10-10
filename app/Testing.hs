{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Testing where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import Behaviour (Behaviour (..), Fun (..), Time, Ô)

myDel :: Time -> a -> Ô a
myDel t x = Delay (singletonClock 0) (const (x :* t))

beh :: Int -> Behaviour Int
beh n = K n :+: myDel n (beh (n + n))

--test_beh n = K n :+: myDel n ((K n+1) :+: (myDel n+1 (beh n)))

beh' :: Int -> Behaviour Int
beh' n = Fun (box id) :+: myDel n (beh' (n + n))

evalFun :: Fun Time t2 -> Time -> t2
evalFun (Fun a) t = unbox a t
evalFun (K a) _t = a

-- Sample every_x_tick amount_of_samples behavior
sample :: (Stable a) => Int -> Behaviour a -> [Time :* a]
sample 0 (b :+: (Delay cl later)) = let (_b' :* t) = later (InputValue 0 cl) in [t :* evalFun b t]
sample amountOfSamples (b :+: (Delay cl later)) = let (b' :* t) = later (InputValue 0 cl) in (t :* evalFun b t) : sample (amountOfSamples - 1) b'
