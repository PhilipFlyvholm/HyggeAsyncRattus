{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Testing where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import Behaviour (Behaviour (..), Fun (..), Time, Ô)
import qualified Data.IntSet as IntSet
import System.Random (RandomGen, uniformR)
import Prelude hiding (max, min)
import Data.Time (getCurrentTime)
import GHC.IO (unsafePerformIO)

{-# NOINLINE getTimeUnsafe #-}
getTimeUnsafe :: Time
getTimeUnsafe = unsafePerformIO getCurrentTime

myDel :: Time -> a -> Ô a
myDel t x = Delay (singletonClock 0) (const (x :* t))

beh :: Int -> Behaviour Int
beh n = K n :+: myDel getTimeUnsafe (beh (n + n))

myDelA :: Time -> a -> Ô a
myDelA t x = Delay (IntSet.fromList [0, 1]) (const (x :* t))

behA :: Int -> Behaviour Int
behA n = K n :+: myDelA getTimeUnsafe (behA (n+n))

myDelB :: Time -> a -> Ô a
myDelB t x = Delay (IntSet.fromList [1, 2]) (const (x :* t))

behB :: Int -> Behaviour Int
behB n = K n :+: myDelB getTimeUnsafe (behB n)

-- test_beh n = K n :+: myDel n ((K n+1) :+: (myDel n+1 (beh n)))

beh' :: Int -> Behaviour Int
beh' n = Fun (box (const n)) :+: myDel getTimeUnsafe (beh' (n + n))

evalFun :: Fun Time t2 -> Time -> t2
evalFun (Fun a) t = unbox a t
evalFun (K a) _t = a

-- Sample every_x_tick amount_of_samples behavior
sample :: (Stable a, RandomGen g) => Int -> Behaviour a -> g -> [Time :* a]
sample 0 (b :+: (Delay cl later)) gen =
  let (randomValue, _) = randomFromIntSetPure cl gen
      (_b' :* t) = later (InputValue randomValue cl)
   in [t :* evalFun b t]
sample amountOfSamples (b :+: (Delay cl later)) gen =
  let (randomValue, newGen) = randomFromIntSetPure cl gen
      (b' :* t) = later (InputValue randomValue cl)
   in (t :* evalFun b t) : sample (amountOfSamples - 1) b' newGen

randomFromIntSetPure :: (RandomGen g) => IntSet.IntSet -> g -> (Int, g)
randomFromIntSetPure intSet gen
  | IntSet.null intSet = (0, gen)
  | otherwise =
      let elems = IntSet.toList intSet
          (randomIdx, newGen) = uniformR (0, length elems - 1) gen
       in (elems !! randomIdx, newGen)