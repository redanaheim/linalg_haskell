{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
import Field
import Vector
import Solvable
import Polynomial
import Expression hiding (main)

class (Vector a, RealField (Scalar a)) => NormedSpace a where
    norm :: a -> Scalar a

instance (Vector a, InnerProduct a, RealField (Scalar a)) => NormedSpace a where
    norm x = Field.exp (midpoint Field.zero (Field.log (inner x x)))

normalize :: (NormedSpace a) => a -> a
normalize x = Vector.fmul x (Field.fminv (norm x))

gram_schmidt :: (InnerProduct a, NormedSpace a) => [a] -> [a]
gram_schmidt x = foldr (\x subspace_basis -> subspace_basis ++ [normalize (Vector.vadd x (Vector.vneg (project subspace_basis x)))]) [] (reverse x)

main = do
    let basis = [[RationalConst (-2), RationalConst (-3), RationalConst 2], [RationalConst 0, RationalConst (-1), RationalConst 1]] :: [[Expression]]
    print (gram_schmidt basis)
    print (fmap (fmap (compute (ComputationContext (const 1)))) (gram_schmidt basis))