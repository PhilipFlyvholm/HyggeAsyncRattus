{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Event where
import AsyncRattus
import Behaviour hiding (map, filter)
import Prelude hiding (map)
import AsyncRattus.Channels (Producer (..))

data Event a = !a :&: !(Ô(Event a))


instance Producer p a => Producer (Ô p) a where
  getCurrent _ = Nothing'
  getNext p cb = cb (delay (let (b :* _) = adv p in b))

instance Producer (Event a) a where
  getCurrent p = Just' (current p)
  getNext p cb = cb (delay (let (b :* _) = adv (future p) in b))

-- | Get the current value of a signal.
current :: Event a -> a
current (x :&: _) = x

-- | Get the future the Event.
future :: Event a -> Ô (Event a)
future (_ :&: xs) = xs

triggerAwait :: (Stable b) => Box(a->b->c) -> Ô(Event a) -> Behaviour b -> Ô(Event(Maybe' c))
triggerAwait f as (b :+: bs) =
    delay (case select as bs of
            Fst (a' :&: as' :* t) bs' -> Just' (unbox f a' (apply b t)) :&: triggerAwait f as' (b :+: bs') :* t
            Snd as' (bs' :* t) -> Nothing' :&: triggerAwait f as' bs' :* t
            Both (a' :&: as' :* at) (b' :+: bs' :* bt) -> Just' (unbox f a' (apply b' bt)) :&: triggerAwait f as' (b' :+: bs') :* max at bt
        )

map :: Box (a -> b) -> Event a -> Event b
map f (a :&: xs) = unbox f a :&: delay (let (b' :* t'') = adv xs in (map f b' :* t''))

filter :: Box (a -> Bool) -> Event a -> Event (Maybe' a)
filter f = map (box (\x -> if unbox f x then Just' x else Nothing'))


--mkEvent :: Box (a -> Ô a) -> Event a
--mkEvent b = delay(let (v :* t) = adv(unbox b) in ((v :* t mkEvent b) :* t))


mkBehaviour :: Event a -> Behaviour a
mkBehaviour (a :&: as) = K a :+: delay (let (b' :* t) = adv as in (mkBehaviour b' :* t))