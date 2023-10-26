{-# Language TypeFamilies, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Field
import Vector hiding (main)
import Data.Kind
import Data.List hiding (transpose)
import Control.Monad (join)
import Data.Maybe
import Text.Printf (printf)

(!?) :: [a] -> Int -> Maybe a

xs !? n
    | n < 0     = Nothing
    | otherwise = foldr (\x r k -> case k of
        0 -> Just x
        _ -> r (k-1)) (const Nothing) xs n

data Matrix a where
    Matrix :: (Field a) => {
        rows :: Int,
        cols :: Int,
        content :: [[a]]
    } -> Matrix a

idx :: Matrix a -> Int -> Int -> Maybe a
idx m i j = join ((<*>) (Prelude.fmap (!?) (content m !? (i - 1))) (Just (j - 1)))

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
    (Just []) [idx m i j | i <- [1 .. rows m]]

instance (Eq a) => Eq (Matrix a) where
    x == y =
        let r1 = rows x
            c1 = cols x
        in and $ (r1 == rows y) : (c1 == cols y) :
            [ idx x i j == idx y i j | i <- [1 .. r1], j <- [1 .. c1]]

class ScalarFunctor f where
    smap :: (Field b) => (a -> b) -> f a -> f b

instance ScalarFunctor [] where
    smap = fmap

instance ScalarFunctor Matrix where
    smap f m = Matrix (rows m) (cols m) (fmap (fmap f) (content m))

instance Show (Matrix a) where
    show (Matrix rows cols x) = show x

fill_by :: (Field a) => Int -> Int -> (Int -> Int -> a) -> Matrix a
fill_by r c f = Matrix r c [[ f i j | j <- [1 .. c]] | i <- [1 .. r]]

id_mat :: (Field a) => Int -> Matrix a
id_mat s = fill_by s s (\i j -> if i == j then Field.one else Field.zero)

mmul :: Field a => Matrix a -> Matrix a -> Matrix a
mmul m1 m2 = fill_by (rows m1) (cols m2) (\i j -> inner (fromJust (row m1 i)) (fromJust (col m2 j)))

transpose m = fill_by (cols m) (rows m) (\i j -> fromJust (idx m j i))

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
    NoneOp :: RowOp a

instance (Field a) => Show (RowOp a) where 
  show op = case op of
    SwapRowOp a b -> printf "swap %s %s" (show a) (show b)
    SetRowOp src tgt c -> printf "set row %s to %s times row %s" (show tgt) (show c) (show src)
    AddRowOp src tgt c -> printf "add %s times row %s to row %s" (show c) (show src) (show tgt)
    NoneOp -> "<no row op>"

swap_int :: Int -> Int -> Int -> Int
swap_int a b i = if i == a then b else (if i == b then a else i)

swap_op :: (Field a) =>  Int -> Int -> Matrix a -> Matrix a
swap_op a b m = fill_by (rows m) (cols m) (\i j -> fromJust (idx m (swap_int a b i) j))

get_idx_set :: (Field a) => Matrix a -> Int -> Int -> a -> Int -> Int -> a
get_idx_set m src tgt c i j = if i == tgt then (Field.fmul (fromJust (idx m src j)) c) else fromJust (idx m i j)

set_op :: (Field a) => Int -> Int -> a -> Matrix a -> Matrix a
set_op src tgt c m = fill_by (rows m) (cols m) (get_idx_set m src tgt c)

add_op :: (Field a) => Int -> Int -> a -> Matrix a -> Matrix a
add_op src tgt c m = fill_by (rows m) (cols m) (\i j -> Field.fadd (get_idx_set m src tgt c i j) (if i == tgt then fromJust (idx m i j) else Field.zero))

do_op :: (Field a) => RowOp a -> Matrix a -> Matrix a
do_op f = case f of
    SwapRowOp a b -> swap_op a b
    SetRowOp src tgt c -> set_op src tgt c
    AddRowOp src tgt c -> add_op src tgt c
    NoneOp -> id

fidx :: (Field a) => Int -> [a] -> a
fidx i list = list !! (i - 1)

-- gets the row op to apply given a matrix column
-- first argument is the row of the current pivot

fix_op :: (Field a) => Int -> Maybe Int -> [a] -> RowOp a
fix_op pivot tgt col = maybe (NoneOp) (\val -> AddRowOp pivot val (Field.fneg (fidx val col))) tgt

idx_first_nonzero_val :: (Field a) => Int -> [a] -> Maybe Int
idx_first_nonzero_val skip list = fmap (+1) (elemIndex True [(fidx i list) /= Field.zero && (i /= skip) | i <- [1..length list]])

fill_pivot_op :: (Field a) => Int -> Maybe Int -> RowOp a
fill_pivot_op pivot tgt = maybe (NoneOp :: RowOp a) (\val -> SwapRowOp pivot val) tgt

make_pivot_one_op :: (Field a) => Int -> [a] -> RowOp a
make_pivot_one_op pivot col = SetRowOp pivot pivot (Field.fminv (fidx pivot col))

get_op :: (Field a) => Int -> [a] -> RowOp a
get_op pivot col 
    | (fidx pivot col) == Field.one = fix_op pivot (idx_first_nonzero_val pivot col) col
    | (fidx pivot col) == Field.zero = fill_pivot_op pivot (idx_first_nonzero_val pivot col)
    | True = make_pivot_one_op pivot col

instance (Field a) => Vector (Matrix a) where
    type Scalar (Matrix a) = a
    vadd x y = fill_by (rows x) (cols x) (\i j -> Field.fadd (fromJust (idx x i j)) (fromJust (idx y i j)))
    vneg = smap Field.fneg
    fmul :: Field a => Matrix a -> Scalar (Matrix a) -> Matrix a
    fmul x c = smap (Field.fmul c) x


main = do
    let m = Matrix 3 3 [[0, 1, 2], [0.5, 2, 3], [2, 2, 3]] :: Matrix Double
    let column = fromJust ((col m (1 :: Int)) :: Maybe [Double])
    print (column)
    print (fidx 1 column)
    print (get_op 1 column)
    print (get_op 1 (fromJust (col (do_op (get_op 1 column) m) 1)))