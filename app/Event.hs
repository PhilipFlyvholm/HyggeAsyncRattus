{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module Event where
import AsyncRattus
import Behaviour hiding (map, filter)
import Prelude hiding (map)

data Event a = !a :&: !(Ô(Event a))

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