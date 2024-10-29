
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
module Behaviour where
import AsyncRattus
import Prelude hiding (zipWith, map)
import AsyncRattus.Channels (getInput)
import Strict (UTCTime', getCurrentStrictTime)
type Time = UTCTime'

type Ô a = O (a :* Time)

data Fun t a = K !a | Fun !(Box (t -> a))

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

data Behaviour a = !(Fun Time a) :+: !(Ô (Behaviour a))

map :: Box (a -> b) -> Behaviour a -> Behaviour b
map f ((K a) :+: xs) = K (unbox f a) :+: delay (let (b' :* t) = adv xs in (map f b' :* t))
map f ((Fun t) :+: xs) = Fun (box (unbox f . unbox t)) :+: delay (let (b' :* t'') = adv xs in (map f b' :* t''))

-- Make it filter events instead or make this into a ceil function
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
-- Box (Ô a) x -> O ()
getInpût :: IO (Box (Ô a) :* (a -> IO ())) 
getInpût =  do 
              (b :* f) <- getInput
              return (b :* \a -> do t <- getCurrentStrictTime
                                    f (a :* t))
