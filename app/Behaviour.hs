{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Behaviour where

import AsyncRattus
import Prelude hiding (map, zipWith)
import StrictUTCTime (diffUTCTime', getCurrentStrictTime)
import Primitives

data Behaviour a = !(Fun Time a) :+: !(OT (Behaviour a))

timeBehaviour :: Behaviour Time
timeBehaviour = Fun (box id) :+: never

map :: Box (a -> b) -> Behaviour a -> Behaviour b
map f (x :+: xs) = applyF f x :+: delay (let (b' :* t) = adv xs in (map f b' :* t))

filter :: Box (a -> Bool) -> Behaviour a -> Behaviour (Maybe' a)
filter f = map (box (\x -> if unbox f x then Just' x else Nothing'))

zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Behaviour a -> Behaviour b -> Behaviour c
zipWith f (x :+: xs) (y :+: ys) =
  app x y
    :+: delay
      ( case select xs ys of
          Fst (xs' :* xst) lys -> zipWith f xs' (y :+: lys) :* xst
          Snd lxs (ys' :* yst) -> zipWith f (x :+: lxs) ys' :* yst
          Both (xs' :* xst) (ys' :* yst) -> zipWith f xs' ys' :* max xst yst
      )
  where
    app (K x') (K y') = K (unbox f x' y')
    app (Fun x') (Fun y') = Fun (box (\t -> unbox f (unbox x' t) (unbox y' t)))
    app (Fun x') (K y') = Fun (box (\t -> unbox f (unbox x' t) y'))
    app (K x') (Fun y') = Fun (box (unbox f x' . unbox y'))

switch :: Behaviour a -> OT (Behaviour a) -> Behaviour a
switch (x :+: xs) d =
  x
    :+: delay
      ( case select xs d of
          Fst (xs' :* t) d' -> switch xs' d' :* t
          Snd _ d' -> d'
          Both _ d' -> d'
      )

startTimerBehaviour :: IO (Behaviour Int)
startTimerBehaviour = do
  start <- getCurrentStrictTime
  let b = K start :+: never
  return $ zipWith (box (\currentTime startTime -> round (diffUTCTime' currentTime startTime))) timeBehaviour b
