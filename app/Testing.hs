
module Testing where
import AsyncRattus.InternalPrimitives
import Main (Behaviour (..), Fun (..))

myDel :: b -> a -> O (a, b)
myDel t x = Delay (singletonClock 0) (const (x, t))

beh :: Int -> Behaviour Int
beh n = K n :+: myDel n (beh (n+n))

beh' :: t -> Behaviour a
beh' n = Fun id :+: mydel n (beh' (n+n))