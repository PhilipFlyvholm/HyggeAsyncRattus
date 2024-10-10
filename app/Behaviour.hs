
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
module Behaviour where
import AsyncRattus
import Prelude hiding (zipWith, map)
type Time = Int

type Ô a = O (a :* Time)

data Fun t a = K !a | Fun !(Box (t -> a))

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

data Behaviour a = (Fun Time a) :+: Ô (Behaviour a)

map :: Box (a -> b) -> Behaviour a -> Behaviour b
map f ((K a) :+: xs) = K (unbox f a) :+: delay (let (b' :* t) = adv xs in (map f b' :* t))
map f ((Fun t) :+: xs) = Fun (box (unbox f . unbox t)) :+: delay (let (b' :* t'') = adv xs in (map f b' :* t''))

filter :: Box (a -> Bool) -> Behaviour a -> Behaviour (Maybe' a)
filter f = map (box (\x -> if unbox f x then Just' x else Nothing'))

zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Behaviour a -> Behaviour b -> Behaviour c
zipWith f (a :+: as) (b :+: bs) =
  app a b :+: delay (
    case select as bs of
      Fst (as' :* ast) lbs -> zipWith f as' (b :+: lbs) :* ast
      Snd las (bs' :* bst) -> zipWith f (a :+: las) bs' :* bst
      Both (as' :* ast) (bs' :* bst) -> zipWith f as' bs' :* max ast bst
  )
  where app (K a') (K b') = K (unbox f a' b')
        app (Fun a') (Fun b') = Fun (box (\t -> unbox f (unbox a' t) (unbox b' t)))
        app (Fun a') (K b') = Fun (box (\t -> unbox f (unbox a' t) b'))
        app (K a') (Fun b') = Fun (box (unbox f a' . unbox b'))


switch :: Behaviour a -> Ô (Behaviour a) -> Behaviour a
switch (x :+: xs) d = x :+: delay (
    case select xs d of
        Fst (xs' :* t) d' -> switch xs' d' :* t
        Snd _ d' -> d'
        Both _ d' -> d'
    )

{- trigger :: (Stable a, Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> IO (Box (Sig c))
trigger f (a ::: as) bs@(b:::_) = do s <- triggerAwait f as bs
                                     return (box (unbox f a b ::: unbox s))
-- | This function is similar to 'trigger' but takes a delayed signal
-- (type @O (Sig a)@) as an argument instead of a signal (@Sig a@).
triggerAwait :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> IO (Box (O (Sig c)))
triggerAwait f as bs = mkBoxSig <$> mkInput (box SigMaybe `mapO` (trig f as bs)) where
  trig :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> O (Sig (Maybe' c))
  trig f as (b ::: bs) =
    delay (case select as bs of
            Fst (a' ::: as') bs' -> Just' (unbox f a' b) ::: trig f as' (b ::: bs')
            Snd as' bs' -> Nothing' ::: trig f as' bs'
            Both (a' ::: as') (b' ::: bs') -> Just' (unbox f a' b') ::: trig f as' (b' ::: bs')
          ) -}

data Event a = !a :&: Ô(Event a)

triggerAwait :: Stable b => Box(a->b->c) -> Ô(Event a) -> Behaviour b -> Ô(Event(Maybe' c))
triggerAwait f as (b :+: bs) = 
    delay (case select as bs of 
            Fst (a' :&: as' :* t) bs' -> Just' (unbox f a' (apply b t)) :&: triggerAwait f as' (b :+: bs') :* t
            Snd as' (bs' :* t) -> Nothing' :&: triggerAwait f as' bs' :* t
            Both (a' :&: as' :* at) (b' :+: bs' :* bt) -> Just' (unbox f a' (apply b' bt)) :&: triggerAwait f as' (b' :+: bs') :* max at bt
        )