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
import Matrix

class (Vector a, RealField (Scalar a)) => NormedSpace a where
    norm :: a -> Scalar a

instance (Vector a, InnerProduct a, RealField (Scalar a)) => NormedSpace a where
    norm x = Field.exp (midpoint Field.zero (Field.log (inner x x)))

normalize :: (NormedSpace a) => a -> a
normalize x = Vector.fmul x (Field.fminv (norm x))

gram_schmidt :: (InnerProduct a, NormedSpace a) => [a] -> [a]
gram_schmidt x = foldr (\x subspace_basis -> subspace_basis ++ [normalize (Vector.vadd x (Vector.vneg (project subspace_basis x)))]) [] (reverse x)

-- q is the first matrix, r is the second
qr_factorize :: (Field a, InnerProduct [a], NormedSpace [a]) => Matrix a -> (Matrix a, Matrix a)
qr_factorize m = do
    let q = risky_columns (gram_schmidt (content (transpose m)))
    let r = change_of_basis_matrix (content (transpose m)) (content (transpose q))
    (q, r)

main = do
    let basis = [[-2, -1, 1], [-2, -5, 7], [-2, -1, -5], [-2, -5, 1]] :: [[Double]]
    let mat = Matrix 4 3 basis
    print mat
    print (fst (qr_factorize mat))
    print (snd (qr_factorize mat))