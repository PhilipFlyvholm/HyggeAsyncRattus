{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Main where

import AsyncRattus ( box )
import AsyncRattus.Signal

sums :: Sig Int -> Sig Int
sums = scan (box (+)) 0

main :: IO ()
main = putStrLn "Hello, Haskell!"
