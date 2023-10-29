{-# Language TypeFamilies, AllowAmbiguousTypes #-}
module Vector where
import Field
import Data.Kind

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

dot :: (Field a) => [a] -> [a] -> a
dot x y = foldr (fadd . uncurry Field.fmul) Field.zero (zip x y)

instance (Field a) => InnerProduct [a] where
    inner = dot