{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Vector where

import Data.Kind
import Field

class (Eq a, Field (Scalar a)) => Vector a where
  type Scalar a :: Type

  vadd :: a -> a -> a
  vneg :: a -> a

  fmul :: a -> Scalar a -> a

class (Vector a) => InnerProduct a where
  inner :: a -> a -> Scalar a

instance Vector Double where
  type Scalar Double = Double
  vadd x y = x + y
  vneg x = x * (-1)
  fmul x y = x * y

instance InnerProduct Double where
  inner :: Double -> Double -> Scalar Double
  inner = Field.fmul

instance (Field a) => Vector [a] where
  type Scalar [a] = a

  vadd x y = fmap (uncurry fadd) (zip x y)
  fmul x c = fmap (`Field.fmul` c) x
  vneg = fmap fneg

ifmap :: (a -> Int -> b) -> [a] -> [b]
ifmap func list = fmap (uncurry func) (zip list (scanl (+) (1 :: Int) (repeat 1)))

dot :: (Field a) => [a] -> [a] -> a
dot x y = foldr (fadd . uncurry Field.fmul) Field.zero (zip x y)

one_half :: (Field a) => a
one_half = Field.fminv (Field.fadd Field.one Field.one)

instance (Field a) => InnerProduct [a] where
  inner = dot

line_project :: (InnerProduct a) => a -> a -> a
line_project unit x = Vector.fmul unit (inner unit x)

project :: (InnerProduct a) => [a] -> a -> a
project orthonormal_basis x = foldr (vadd . (`line_project` x)) (Vector.fmul x Field.zero) orthonormal_basis

std_coordinates :: (Field a) => Int -> [[a]]
std_coordinates len = [[(if i == j then Field.one else Field.zero) | j <- [1 .. len]] | i <- [1 .. len]]
