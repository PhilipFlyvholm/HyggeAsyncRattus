{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Behaviour where

import AsyncRattus
import AsyncRattus.Channels (Producer, getInput, setOutput)
import Prelude hiding (map, zipWith)
import StrictUTCTime (UTCTime', diffUTCTime', getCurrentStrictTime)
import Primitives

data Behaviour a = !(Fun Time a) :+: !(OT (Behaviour a))

timeBehaviour :: Behaviour Time
timeBehaviour = Fun (box id) :+: never

map :: Box (a -> b) -> Behaviour a -> Behaviour b
map f ((K a) :+: xs) = K (unbox f a) :+: delay (let (b' :* t) = adv xs in (map f b' :* t))
map f ((Fun t) :+: xs) = Fun (box (unbox f . unbox t)) :+: delay (let (b' :* t'') = adv xs in (map f b' :* t''))

filter :: Box (a -> Bool) -> Behaviour a -> Behaviour (Maybe' a)
filter f = map (box (\x -> if unbox f x then Just' x else Nothing'))

zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Behaviour a -> Behaviour b -> Behaviour c
zipWith f (a :+: as) (b :+: bs) =
  app a b
    :+: delay
      ( case select as bs of
          Fst (as' :* ast) lbs -> zipWith f as' (b :+: lbs) :* ast
          Snd las (bs' :* bst) -> zipWith f (a :+: las) bs' :* bst
          Both (as' :* ast) (bs' :* bst) -> zipWith f as' bs' :* max ast bst
      )
  where
    app (K a') (K b') = K (unbox f a' b')
    app (Fun a') (Fun b') = Fun (box (\t -> unbox f (unbox a' t) (unbox b' t)))
    app (Fun a') (K b') = Fun (box (\t -> unbox f (unbox a' t) b'))
    app (K a') (Fun b') = Fun (box (unbox f a' . unbox b'))

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