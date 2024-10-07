{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal
import SigMaybe
import Prelude hiding (init)
import AsyncRattus.Strict

type Time = Integer

type Ô a = O (a :* Time)

data Fun t a = K !a | Fun !(Box (t -> a))

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

data Behaviour a = (Fun Time a) :+: Ô (Behaviour a)


-- adv' :: Ô a -> InputValue -> a
-- adv' (Delay f) inp = f inp

mapB :: Box (a -> b) -> Behaviour a -> Behaviour b
mapB f ((K a) :+: xs) = (K (unbox f a)) :+: delay (let (b' :* t) = adv xs in (mapB f b' :* t))
mapB f ((Fun t) :+: xs) = Fun (box (\t' -> unbox f (unbox t t'))) :+: delay (let (b' :* t'') = adv xs in (mapB f b' :* t''))

-- mFilter :: Box (a -> Bool) -> SigMaybe a -> SigMaybe a
-- mFilter f (Just' a ::: xs) = (if unbox f a then Just' a else Nothing') ::: de  lay (mFilter f (adv xs))
-- mFilter f (Nothing' ::: xs) = Nothing' ::: delay (mFilter f (adv xs))

filterB :: Box (a -> Bool) -> Behaviour a -> Behaviour (Maybe' a)
filterB f = mapB (box (\x -> if unbox f x then Just' x else Nothing'))

{- zipWith :: (Stable a, Stable b) => Box(a -> b -> c) -> Sig a -> Sig b -> Sig c
zipWith f (a ::: as) (b ::: bs) = unbox f a b ::: delay (
    case select as bs of
      Fst as' lbs -> zipWith f as' (b ::: lbs)
      Snd las bs' -> zipWith f (a ::: las) bs'
      Both as' bs' -> zipWith f as' bs'
  ) -}


zipWithB :: (Stable a, Stable b) => Box (a -> b -> c) -> Behaviour a -> Behaviour b -> Behaviour c
zipWithB f (a :+: as) (b :+: bs) = 
  app a b :+: delay (
    case select as bs of
      Fst (as' :* ast) lbs -> zipWithB f as' (b :+: lbs) :* ast
      Snd las (bs' :* bst) -> zipWithB f (a :+: las) bs' :* bst
      Both (as' :* ast) (bs' :* bst) -> zipWithB f as' bs' :* (max ast bst)
  )
  where app (K a') (K b') = K (unbox f a' b')
        app (Fun a') (Fun b') = Fun (box (\t -> unbox f (unbox a' t) (unbox b' t)))



main :: IO ()
main = do
  showNat <- trigger (box (\_ n -> n)) everySecondSig (mMap (box (+ 1)) (nats 0))

  setPrint showNat
  startEventLoop
