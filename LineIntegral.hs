{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

import Data.Bifunctor qualified
import Field
import Vector

type OneForm a b = a -> (a, a) -> b

type Differential a b = (a, a) -> b

type Point = (Double, Double)

dmap :: (a -> b) -> (a, a) -> (b, b)
dmap f = Data.Bifunctor.bimap f f

dfmap :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
dfmap f a = Data.Bifunctor.bimap (f (fst a)) (f (snd a))

instance Vector Point where
  type Scalar Point = Double
  fmul x c = dmap (c *) x
  vadd = dfmap (+)
  vneg = dmap ((-1) *)

dx :: Differential Point Double
dx vs = fst (snd vs) - fst (fst vs)

dy :: Differential Point Double
dy vs = snd (snd vs) - snd (fst vs)

total :: Point -> Double
total = uncurry (+)

magnitude :: Point -> Double
magnitude v = sqrt (total (dmap (\a -> a * a) v))

arc_length :: Point -> Point -> Double
arc_length a b = magnitude (Vector.vadd a (Vector.vneg b))

ds :: Differential Point Double
ds = uncurry arc_length

one_form :: (Vector a, Field b) => (a -> b) -> Differential a b -> OneForm a b
one_form f da v vs = Field.fmul (f v) (da vs)

both :: (Field b) => OneForm a b -> OneForm a b -> OneForm a b
both p q v vs = Field.fadd (p v vs) (q v vs)

type Curve a b = b -> a

type Range b = (b, b)

class Ordered a where
  less_than :: a -> a -> Bool
  greater_than :: a -> a -> Bool

instance Ordered Double where
  less_than a b = a < b
  greater_than a b = a > b

line_integral :: (Vector a, Field b, Field c, Ordered c) => OneForm a b -> Curve a c -> Range c -> c -> b
line_integral f c r s = do
  let init = fst r
  let end = snd r
  if not (less_than init end)
    then Field.zero
    else do
      let vs = dmap c (init, Field.fadd init s)
      let midpoint = Vector.fmul (uncurry Vector.vadd vs) one_half
      Field.fadd (f midpoint vs) (line_integral f c (Field.fadd init s, end) s)

main = do
  let f v = fst v * (snd v * snd v * snd v * snd v)
  let c t = (4 * cos t, 4 * sin t)
  let r = (-pi / 2, pi / 2) :: (Double, Double)
  let s = 0.0001 :: Double
  print (line_integral (one_form f ds) c r s)
