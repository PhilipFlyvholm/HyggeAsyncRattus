{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Testing where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import Main (Behaviour (..), Fun (..), Time, Ô)

myDel :: Time -> a -> Ô a
myDel t x = Delay (singletonClock 0) (const (x :* t))

beh :: Integer -> Behaviour Integer
beh n = K n :+: myDel n (beh (n + n))

beh' :: Integer -> Behaviour Integer
beh' n = Fun (box id) :+: myDel n (beh' (n + n))

evalFun :: Fun Time t2 -> Time -> t2
evalFun (Fun a) t = unbox a t
evalFun (K a) _t = a

-- Sample every_x_tick amount_of_samples behavior
sample :: Integer -> Behaviour a -> [Time :* a]
sample 0 (b :+: (Delay cl f)) = let (_b' :* t) = f (InputValue 0 cl) in [t :* evalFun b t]
sample amountOfSamples (b :+: (Delay cl f)) = let (b' :* t) = f (InputValue 0 cl) in (t :* evalFun b t) : sample (amountOfSamples - 1) b'
