{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DatatypeContexts #-}
module Solvable where
import Vector
import Field
import Util
import Data.Ratio (Ratio)
import Matrix
import Data.List (elemIndex)
import Data.Maybe (isNothing, fromJust)

class (Vector a) => Solvable a where
    -- viewing [a] as a basis, returns the coefficient of the basis element specified by the Int in the basis expansion of an element
    -- index integer starts from 1
    get_coord :: [a] -> a -> Int -> Scalar a

basis_coords :: (Solvable a) => [a] -> a -> [Scalar a]
basis_coords basis x = fmap (get_coord basis x) [1..length basis]

evaluate_basis_coords :: (Solvable a) => [a] -> [Scalar a] -> a
-- artifact of the fact that soem vector implementations are not really vector spaces
-- they can't have one unique zero vector so to get the right size zero vector we use Vector.fmul
evaluate_basis_coords basis coords = foldr vadd (Vector.fmul (head basis) Field.zero) (zipWith Vector.fmul basis coords)

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
    get_coord basis vector index = fidx index (do_ops_col (row_ops_log (rref (from_columns (length basis) (length (head basis)) basis))) vector)

instance (Field a) => Solvable (Matrix a) where
    get_coord basis vector = get_coord (fmap flatten basis) (flatten vector)

data LinearTransformation a b where
    LinearTransformation :: (Vector a, Vector b) => { morphism :: a -> b } -> LinearTransformation a b

get_matrix :: (Solvable a, Solvable b, Scalar a ~ Scalar b) => [a] -> [b] -> LinearTransformation a b -> Matrix (Scalar b)
get_matrix basis_a basis_b transform = risky_columns [basis_coords basis_b (morphism transform (fidx i basis_a)) | i <- [1..(length basis_a)]]

get_image_basis :: (Solvable a) => [a] -> LinearTransformation a a -> [[Scalar a]]
get_image_basis basis transform = do
    let matrix = get_matrix basis basis transform
    let reduced_matrix = rref matrix
    let transposed = transpose matrix
    [ fidx i (content transposed) | i <- pivots_list reduced_matrix ]

get_kernel_basis_component :: (Field a) => Matrix a -> [Int] -> Int -> Int -> a
get_kernel_basis_component m pivots c free_column = do
    let row_idx_maybe = fmap (+ 1) (elemIndex c pivots)
    if isNothing row_idx_maybe then (if c == free_column then Field.one else Field.zero) else
        (do
            let row_idx = fromJust row_idx_maybe
            Field.fneg (fromJust (midx m row_idx free_column))
        )

get_kernel_basis :: (Solvable a) => [a] -> LinearTransformation a a -> [[Scalar a]]
get_kernel_basis basis transform = do
    let matrix = get_matrix basis basis transform
    let reduced_matrix = rref matrix
    let transposed = transpose matrix
    let free_columns = filter (\i -> i `notElem` pivots_list reduced_matrix) [1..(cols matrix)]
    [[get_kernel_basis_component (matrix_result reduced_matrix) (pivots_list reduced_matrix) c f | c <- [1..cols matrix]] | f <- free_columns]

change_of_basis_matrix :: (Solvable a) => [a] -> [a] -> Matrix (Scalar a)
change_of_basis_matrix basis_a basis_b = get_matrix basis_a basis_b (LinearTransformation id)