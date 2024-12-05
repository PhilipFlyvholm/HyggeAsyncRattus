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
import Primitives

-- | An event is a discrete time/value pairs 
data Event a = !a :&: !(OT (Event a))

-- | Event wrapper for Maybe'
newtype EventMaybe a = EventMaybe (Event (Maybe' a))

instance Producer (EventMaybe a :* t) a where
  getCurrent (EventMaybe p :* _) = current p
  getNext (EventMaybe p :* _) cb = cb (delay (let (b :* _) = adv (future p) in EventMaybe b))

instance Producer (EventMaybe a) a where
  getCurrent (EventMaybe p) = current p
  getNext (EventMaybe p) cb = cb (delay (let (b :* _) = adv (future p) in EventMaybe b))

instance Producer (Event a) a where
  getCurrent p = Just' (current p)
  getNext p cb = cb (delay (let (b :* _) = adv (future p) in b))

instance Producer (Event a :* Time) a where
  getCurrent (p :* _) = Just' (current p)
  getNext (p :* _) cb = cb (delay (let (b :* _) = adv (future p) in b))

-- | Get the current value of a Event.
current :: Event a -> a
current (x :&: _) = x

-- | Get the future the Event.
future :: Event a -> OT (Event a)
future (_ :&: xs) = xs

-- | Trigger an behaviour when a event occurs.
trigger :: (Stable a, Stable b) => Box (a -> b -> c) -> Event a -> Behaviour b -> IO (Box (Event c))
trigger f (a :&: as) bs@(b :+: _) = do
  s <- triggerAwait f as bs
  let l = box (unbox f a (apply b getTimeUnsafe))
  return (box (unbox l :&: unbox s))


-- make triggerAwaitIO
triggerAwait :: (Stable b) => Box (a -> b -> c) -> OT (Event a) -> Behaviour b -> IO (Box (OT (Event c)))
triggerAwait f a bb = mkInputEvent $ mapOT (box EventMaybe) (trig f a bb) where
  trig :: (Stable b) => Box (a -> b -> c) -> OT (Event a) -> Behaviour b -> OT (Event (Maybe' c))
  trig f' as (b :+: bs) =
    delay
      ( case select as bs of
          Fst (a' :&: as' :* t) bs' -> Just' (unbox f' a' (apply b t)) :&: trig f' as' (b :+: bs') :* t
          Snd as' (bs' :* t) -> Nothing' :&: trig f' as' bs' :* t
          Both (a' :&: as' :* at) (b' :+: bs' :* bt) -> Just' (unbox f' a' (apply b' bt)) :&: trig f' as' (b' :+: bs') :* max at bt
      )

map :: Box (a -> b) -> Event a -> Event b
map f (a :&: xs) = unbox f a :&: delay (let (b' :* t'') = adv xs in (map f b' :* t''))

filterMap :: Box (a -> Maybe' b) -> Event a -> IO (Box (OT (Event b)))
filterMap f event = mkInputEvent (EventMaybe (map f event))

filterMapAwait :: Box (a -> Maybe' b) -> OT (Event a) -> IO (Box (OT (Event b)))
filterMapAwait f event = mkInputEvent (delay (let (e :* _) = adv event in EventMaybe (map f e)))

filter :: Box (a -> Bool) -> Event a -> IO (Box (OT (Event a)))
filter f = filterMap (box (\x -> if unbox f x then Just' x else Nothing'))

filterAwait :: Box (a -> Bool) -> OT (Event a) -> IO (Box (OT (Event a)))
filterAwait f = filterMapAwait (box (\x -> if unbox f x then Just' x else Nothing'))

mkEvent :: Box (OT a) -> OT (Event a)
mkEvent b = delay (let (v :* t) = adv (unbox b) in ((v :&: mkEvent b) :* t))

mkBoxEvent :: Box (OT a) -> Box (OT (Event a))
mkBoxEvent b = box (mkEvent b)

stepper :: Event a -> Behaviour a
stepper (a :&: as) = K a :+: delay (let (b' :* t) = adv as in (stepper b' :* t))

stepperAwait :: OT (Event a) -> OT (Behaviour a)
stepperAwait as = delay (let (e :* t) = adv as in (stepper e :* t))


timer :: Int -> Box (OT ())
timer d = Box (Delay (singletonClock (d `max` 10)) (\_ -> () :* getTimeUnsafe))

everySecondEvent :: Event ()
everySecondEvent = () :&: mkEvent everySecond

everySecond :: Box (OT ())
everySecond = timer 1000000

getInputEvent :: IO (Box (OT (Event a)) :* (a -> IO ()))
getInputEvent = do
  (b :* f) <- getInputWithTime
  return (mkBoxEvent b :* f)

mkInputEvent :: (Producer p a) => p -> IO (Box (OT (Event a)))
mkInputEvent p = mkBoxEvent <$> mkInputWithTime p
