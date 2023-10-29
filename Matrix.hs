{-# Language TypeFamilies, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Matrix where
import Field
import Vector hiding (main)
import Data.Kind
import Data.List hiding (partition, transpose)
import Control.Monad (join)
import Data.Maybe
import Text.Printf (printf)
import Data.Ratio (Ratio)
import Util
import Data.Sequence (chunksOf)

data Matrix a where
    Matrix :: (Field a) => {
        rows :: Int,
        cols :: Int,
        content :: [[a]]
    } -> Matrix a

idx :: [[a]] -> Int -> Int -> Maybe a
idx content i j = join ((<*>) (Prelude.fmap (!?) (content !? (i - 1))) (Just (j - 1)))

midx :: Matrix a -> Int -> Int -> Maybe a
midx m = idx (content m)

row :: Matrix a -> Int -> Maybe [a]
row m i = content m !? (i - 1)

col :: Matrix a -> Int -> Maybe [a]
col m j = foldr (
        \x y ->
            maybe (Nothing :: Maybe [a]) (
                \y_contents ->
                    maybe (Nothing :: Maybe [a]) (
                        \x_contents -> Just (x_contents:y_contents)
                    ) x
            ) y
    )
    (Just []) [midx m i j | i <- [1 .. rows m]]

instance (Eq a) => Eq (Matrix a) where
    x == y =
        let r1 = rows x
            c1 = cols x
        in and $ (r1 == rows y) : (c1 == cols y) :
            [ midx x i j == midx y i j | i <- [1 .. r1], j <- [1 .. c1]]

class ScalarFunctor f where
    smap :: (Field b) => (a -> b) -> f a -> f b

instance ScalarFunctor [] where
    smap = fmap

instance ScalarFunctor Matrix where
    smap f m = Matrix (rows m) (cols m) (fmap (fmap f) (content m))

rep_str :: Int -> String -> String
rep_str n s = concat (replicate n s)

extend_str :: Int -> String -> String
extend_str len x = if length x < len then x ++ rep_str (len - length x) " " else x

print_col :: (Field a) => Int -> Matrix a -> Maybe [String]
print_col col_idx m = do
    c <- col m col_idx
    let max_length = foldr (max . length . show) 0 c
    return (map (extend_str (max_length + 5) . show) c)

instance (Field a) => Show (Matrix a) where
    show m = "\n" ++ unlines (foldr (\cidx bufs -> zipWith (++) (fromJust (print_col cidx m)) bufs) (replicate (rows m) ([] :: String)) [1..cols m])

fill_by :: (Field a) => Int -> Int -> (Int -> Int -> a) -> Matrix a
fill_by r c f = Matrix r c [[ f i j | j <- [1 .. c]] | i <- [1 .. r]]

from_rows :: (Field a) => Int -> Int -> [[a]] -> Matrix a
from_rows = Matrix

risky_rows :: (Field a) => [[a]] -> Matrix a
risky_rows c = Matrix (length c) (length (head c)) c

from_columns :: (Field a) => Int -> Int -> [[a]] -> Matrix a
from_columns r c content = fill_by r c (\i j -> fromJust (idx content j i))

risky_columns :: (Field a) => [[a]] -> Matrix a
risky_columns c = from_columns (length (head c)) (length c) c

id_mat :: (Field a) => Int -> Matrix a
id_mat s = fill_by s s (\i j -> if i == j then Field.one else Field.zero)

mat_std_basis_el :: (Field a) => Int -> Int -> Int -> Int -> Matrix a
mat_std_basis_el r c row_idx col_idx = fill_by r c (\i j -> if (i,j) == (row_idx, col_idx) then Field.one else Field.zero)

mat_std_basis :: (Field a) => Int -> Int -> [Matrix a]
mat_std_basis r c = concat ([[mat_std_basis_el r c i j | j <- [1..c]] | i <- [1..r]])

mmul :: Field a => Matrix a -> Matrix a -> Matrix a
mmul m1 m2 = fill_by (rows m1) (cols m2) (\i j -> dot (fromJust (row m1 i)) (fromJust (col m2 j)))

transpose m = fill_by (cols m) (rows m) (\i j -> fromJust (midx m j i))

flatten :: (Field a) => Matrix a -> [a]
flatten = concat . content

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

unflatten :: (Field a) => Int -> Int -> [a] -> Matrix a
unflatten r c l = from_rows r c (partition c l)

data RowOp a where
    SwapRowOp :: {
        row_one :: Int,
        row_two :: Int
    } -> RowOp a
    SetRowOp :: (Field a) => {
        row_src :: Int,
        row_tgt :: Int,
        multiple_src :: a
    } -> RowOp a
    AddRowOp :: (Field a) => {
        row_src :: Int,
        row_tgt :: Int,
        multiple_src :: a
    } -> RowOp a
    MovePivot :: RowOp a
    MoveColumn :: RowOp a
    Done :: RowOp a

instance (Field a) => Show (RowOp a) where
  show op = case op of
    SwapRowOp a b -> printf "swap %s %s" (show a) (show b)
    SetRowOp src tgt c -> printf "set row %s to %s times row %s" (show tgt) (show c) (show src)
    AddRowOp src tgt c -> printf "add %s times row %s to row %s" (show c) (show src) (show tgt)
    MovePivot -> "<move pivot>"
    MoveColumn -> "<move column>"
    Done -> "<no row op>"

swap_int :: Int -> Int -> Int -> Int
swap_int a b i
  | i == a = b
  | i == b = a
  | otherwise = i

swap_op :: (Field a) =>  Int -> Int -> Matrix a -> Matrix a
swap_op a b m = fill_by (rows m) (cols m) (\i j -> fromJust (midx m (swap_int a b i) j))

get_idx_set :: (Field a) => Matrix a -> Int -> Int -> a -> Int -> Int -> a
get_idx_set m src tgt c i j = if i == tgt then Field.fmul (fromJust (midx m src j)) c else fromJust (midx m i j)

set_op :: (Field a) => Int -> Int -> a -> Matrix a -> Matrix a
set_op src tgt c m = fill_by (rows m) (cols m) (get_idx_set m src tgt c)

add_op :: (Field a) => Int -> Int -> a -> Matrix a -> Matrix a
add_op src tgt c m = fill_by (rows m) (cols m) (\i j -> Field.fadd (get_idx_set m src tgt c i j) (if i == tgt then fromJust (midx m i j) else Field.zero))

do_op :: (Field a) => RowOp a -> Matrix a -> Matrix a
do_op f = case f of
    SwapRowOp a b -> swap_op a b
    SetRowOp src tgt c -> set_op src tgt c
    AddRowOp src tgt c -> add_op src tgt c
    MovePivot -> id
    MoveColumn -> id
    Done -> id

do_ops :: (Field a) => [RowOp a] -> Matrix a -> Matrix a
do_ops ops mat = foldr do_op mat ops

do_op_col :: (Field a) => RowOp a -> [a] -> [a]
do_op_col op c = fromJust (col (do_op op (from_columns (length c) 1 [c])) 1)

do_ops_col :: (Field a) => [RowOp a] -> [a] -> [a]
do_ops_col ops c = foldr do_op_col c ops

-- gets the row op to apply given a matrix column
-- first argument is the row of the current pivot

fix_op :: (Field a) => Int -> Maybe Int -> [a] -> RowOp a
fix_op pivot tgt col = maybe MovePivot (\val -> AddRowOp pivot val (Field.fneg (fidx val col))) tgt

idx_first_nonzero_val :: (Field a) => (Int -> Bool) -> [a] -> Maybe Int
idx_first_nonzero_val skip list = fmap (+1) (elemIndex True [fidx i list /= Field.zero && not (skip i) | i <- [1..length list]])

fill_pivot_op :: (Field a) => Int -> Maybe Int -> RowOp a
fill_pivot_op pivot = maybe (MoveColumn :: RowOp a) (SwapRowOp pivot)

make_pivot_one_op :: (Field a) => Int -> [a] -> RowOp a
make_pivot_one_op pivot col = SetRowOp pivot pivot (Field.fminv (fidx pivot col))

get_op :: (Field a) => Int -> [a] -> RowOp a
get_op pivot col
    | pivot > length col = Done
    | fidx pivot col == Field.one = fix_op pivot (idx_first_nonzero_val (== pivot) col) col
    | fidx pivot col == Field.zero = fill_pivot_op pivot (idx_first_nonzero_val (<= pivot) col)
    | otherwise = make_pivot_one_op pivot col

partial_rref :: (Field a) => Int -> Int -> ([RowOp a], Matrix a) -> ([RowOp a], Matrix a)

partial_rref pivot col_idx state = do
    let log = fst state
    let mat = snd state
    let c = col mat col_idx
    maybe state (
        \val ->
        do
            let op = get_op pivot val
            case op of
                MovePivot -> partial_rref (pivot + 1) (col_idx + 1) (op:log, do_op op mat)
                MoveColumn -> partial_rref pivot (col_idx + 1) (op:log, do_op op mat)
                Done -> state
                _ -> partial_rref pivot col_idx (op:log, do_op op mat)

        ) c

rref :: (Field a) => Matrix a -> ([RowOp a], Matrix a)
rref m = partial_rref 1 1 ([], m)

dsmap :: (Field a, Field b, Field c) => (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
dsmap f m1 m2 = fill_by (rows m1) (cols m1) (\i j -> f (fromJust (midx m1 i j)) (fromJust (midx m2 i j)))

instance (Field a) => Vector (Matrix a) where
    type Scalar (Matrix a) = a
    vadd = dsmap Field.fadd
    vneg = smap Field.fneg
    fmul x c = smap (Field.fmul c) x

instance (Field a) => InnerProduct (Matrix a) where
    inner x y = foldr (\row_idx total -> Field.fadd (dot (fromJust (row x row_idx)) (fromJust (row y row_idx))) total) Field.zero [1..(rows x)]
