{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ZippedWithTest where

import AsyncRattus
import AsyncRattus.Channels
import Behaviour
import StrictUTCTime (getCurrentStrictTime)
import System.Random (StdGen, mkStdGen)
import Testing
import Prelude hiding (init)



gen :: StdGen
gen = mkStdGen 137

printSampled :: (Show a) => [Time :* a] -> IO()
printSampled = mapM_ (putStrLn . (\(t :* a) -> show t ++ " = " ++ show a))

zippedWithTest :: IO ()
zippedWithTest = do
  let rawBeh = beh 1
  let zippedBeh = Behaviour.zipWith (box (+)) rawBeh (beh' 1)

  currentTime <- getCurrentStrictTime
  print ("Current time: " ++ show currentTime)
  print "Sampled behaviour:"
  printSampled (sample 5 5 zippedBeh gen)
  startEventLoop
