{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Event where

import AsyncRattus
import AsyncRattus.Channels (Producer, getCurrent, getNext)
import AsyncRattus.InternalPrimitives (Box (Box), O (..), singletonClock)
import Behaviour hiding (filter, map)
import Testing (getTimeUnsafe)
import Prelude hiding (map)

data Event a = !a :&: !(Ô (Event a))

-- instance (Producer p a) => Producer (Ô p) a where
--   getCurrent _ = Nothing'
--   getNext p cb = cb (delay (let (b :* _) = adv p in b))

newtype EventMaybe a = EventMaybe (Event (Maybe' a))

instance Producer (EventMaybe a) a where
  getCurrent (EventMaybe p) = current p
  getNext (EventMaybe p) cb = cb (delay (let (b :* _) = adv (future p) in EventMaybe b))

instance Producer (Event a) a where
  getCurrent p = Just' (current p)
  getNext p cb = cb (delay (let (b :* _) = adv (future p) in b))

instance Producer (Event a :* Time) a where
  getCurrent (p :* _) = Just' (current p)
  getNext (p :* _) cb = cb (delay (let (b :* _) = adv (future p) in b))

-- | Get the current value of a signal.
current :: Event a -> a
current (x :&: _) = x

-- | Get the future the Event.
future :: Event a -> Ô (Event a)
future (_ :&: xs) = xs

trigger :: (Stable a, Stable b) => Box (a -> b -> c) -> Event a -> Behaviour b -> IO (Box (Event (Maybe' c)))
trigger f (a :&: as) bs@(b :+: _) = do
  s <- triggerAwaitIO f as bs
  -- t <- getCurrentStrictTime
  let l = box (unbox f a (apply b getTimeUnsafe))
  return (box (Just' (unbox l) :&: unbox s))

triggerAwait :: (Stable b) => Box (a -> b -> c) -> Ô (Event a) -> Behaviour b -> Ô (Event (Maybe' c))
triggerAwait f as (b :+: bs) =
  delay
    ( case select as bs of
        Fst (a' :&: as' :* t) bs' -> Just' (unbox f a' (apply b t)) :&: triggerAwait f as' (b :+: bs') :* t
        Snd as' (bs' :* t) -> Nothing' :&: triggerAwait f as' bs' :* t
        Both (a' :&: as' :* at) (b' :+: bs' :* bt) -> Just' (unbox f a' (apply b' bt)) :&: triggerAwait f as' (b' :+: bs') :* max at bt
    )

-- make triggerAwaitIO
triggerAwaitIO :: (Stable b) => Box (a -> b -> c) -> Ô (Event a) -> Behaviour b -> IO (Box (Ô (Event (Maybe' c))))
triggerAwaitIO f as bs = do
  let trigResult = triggerAwait f as bs
  result <- mkInpût trigResult
  return (box (mkEvent result))

map :: Box (a -> b) -> Event a -> Event b
map f (a :&: xs) = unbox f a :&: delay (let (b' :* t'') = adv xs in (map f b' :* t''))

filter :: Box (a -> Bool) -> Event a -> IO (Box (Ô (Event a)))
filter f event = mkInputEvent (EventMaybe (map (box (\x -> if unbox f x then Just' x else Nothing')) event))


filterAwait :: Box (a -> Bool) -> Ô (Event a) -> IO (Box (Ô (Event a)))
filterAwait f event = mkInputEvent (delay (let (e :* _) = adv event in EventMaybe (map (box (\x -> if unbox f x then Just' x else Nothing')) e)))

-- filterMapAwait :: Box (a -> Maybe' b) -> Ô (Event a) -> IO (Box (Ô (Event b)))
-- filterMap f s = mkInputEvent ()

mkEvent :: Box (Ô a) -> Ô (Event a)
mkEvent b = delay (let (v :* t) = adv (unbox b) in ((v :&: mkEvent b) :* t))

mkBoxEvent :: Box (Ô a) -> Box (Ô (Event a))
mkBoxEvent b = box (mkEvent b)

mkBehaviour :: Event a -> Behaviour a
mkBehaviour (a :&: as) = K a :+: delay (let (b' :* t) = adv as in (mkBehaviour b' :* t))

timer :: Int -> Box (Ô ())
timer d = Box (Delay (singletonClock (d `max` 10)) (\_ -> () :* getTimeUnsafe))

everySecondEvent :: Event ()
everySecondEvent = () :&: mkEvent everySecond

everySecond :: Box (Ô ())
everySecond = timer 1000000

getInputEvent :: IO (Box (Ô (Event a)) :* (a -> IO ()))
getInputEvent = do
  (b :* f) <- getInpût
  return (mkBoxEvent b :* f)

mkInputEvent :: (Producer p a) => p -> IO (Box (Ô (Event a)))
mkInputEvent p = mkBoxEvent <$> mkInpût p
