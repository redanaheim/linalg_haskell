{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DatatypeContexts #-}
import Vector
import Field
import Util
import Data.Ratio (Ratio)
import Matrix

class (Vector a) => Solvable a where
    -- viewing [a] as a basis, returns the coefficient of the basis element specified by the Int in the basis expansion of an element
    -- index integer starts from 1
    get_coord :: [a] -> a -> Int -> Scalar a

basis_coords :: (Solvable a) => [a] -> a -> [Scalar a]
basis_coords basis x = fmap (get_coord basis x) [1..length basis]

data Isomorphism a b where
    Isomorphism :: (Vector a, Vector b) => { forward :: a -> b, backward :: b -> a } -> Isomorphism a b

-- index integer starts at 1
get_coord_through_isomorphism :: (Solvable a, InnerProduct b) => Isomorphism a b -> [b] -> b -> Int -> Scalar b
get_coord_through_isomorphism t basis_b element_b basis_index = do
    let el = fidx basis_index
    let basis_a = fmap (backward t) basis_b
    let element_a = backward t element_b
    let coord_a = get_coord basis_a element_a basis_index
    let basis_element_a = backward t (el basis_b)
    let basis_element_b = el basis_b
    let component_a = Vector.fmul basis_element_a coord_a
    let component_b = forward t component_a
    Field.fmul (inner component_b basis_element_b) (Field.fminv (inner basis_element_b basis_element_b))

instance (Field a) => Solvable [a] where
    get_coord basis vector index = fidx index (do_ops_col (fst (rref (from_columns (length basis) (length (head basis)) basis))) vector)

instance (Field a) => Solvable (Matrix a) where
    get_coord basis vector = get_coord (fmap flatten basis) (flatten vector)

main = do
    let t = Isomorphism transpose transpose :: Isomorphism (Matrix (Ratio Integer)) (Matrix (Ratio Integer))
    let m = from_rows 3 2 [[1, 4], [2, 6], [8, 5]] :: Matrix (Ratio Integer)
    let basis_a = [risky_rows [[1, 2], [1, 0], [2, 0]], risky_rows [[2, 1], [1, 0], [2, 0]], risky_rows [[3, 1], [4, 1], [5, 1]], risky_rows [[2, 1], [1, 0], [0, 2]], risky_rows [[2, 1], [0, 1], [0, 2]], risky_rows [[1, 1], [1, 1], [1, 1]]]
    print (basis_coords basis_a m)