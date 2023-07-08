{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Use tuple-section" #-}
module Random where

import Control.Monad (ap)

type Gen = Int

next :: Gen -> Gen
next g =
  ((a * g + c) `mod` m) `div` (2^15)
  where
    a = 25214903917
    c = 11
    m = 2^48

newtype JavaRandom a
  = JavaRandom (Gen -> (Gen, a))
  deriving (Functor)

instance Applicative JavaRandom where
  pure x = JavaRandom (\g -> (g, x))
  (<*>) = ap

instance Monad JavaRandom where
  return = pure
  JavaRandom randomA >>= f =
    JavaRandom randomB
    where
      randomB g = (g'', y)
        where
          (g', x) = randomA g
          (g'', y) =
            runJavaRandom (f x) g'

runJavaRandom :: JavaRandom a -> Gen -> (Gen, a)
runJavaRandom (JavaRandom rand) = rand

randomInt :: JavaRandom Int
randomInt = JavaRandom (\g -> (next g, g))
