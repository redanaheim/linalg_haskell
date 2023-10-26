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

class (Vector a) => Solvable a where
    solve :: [a] -> a -> Maybe (Scalar a) -- takes a basis of the vector space V, an element of the basis, and returns the coefficient in the expansion

instance Vector Double where
    type Scalar Double = Double
    vadd x y = x + y
    vneg x = -1.0 * x
    fmul x y = x * y

instance InnerProduct Double where
    inner x y = Field.fmul x y

instance (Field a) => Vector [a] where
    type Scalar [a] = a
    vadd x y = fmap (\el -> uncurry fadd el) (zip x y)
    vneg x = fmap (\el -> fneg el) x
    fmul x c = fmap (\el -> Field.fmul el c) x

instance (Field a) => InnerProduct [a] where
  inner x y = foldr fadd Field.zero (fmap (\el -> uncurry Field.fmul el) (zip x y))

main = do
    print (inner (1.0 :: Double) (4.5 :: Double))
