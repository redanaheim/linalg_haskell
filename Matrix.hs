{-# Language TypeFamilies, AllowAmbiguousTypes #-}
import Field
import Vector hiding (main)
import Data.Kind
import Data.List hiding (transpose)
import Control.Monad (join)
import Data.Maybe

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

instance (Field a) => Vector (Matrix a) where 
    type Scalar (Matrix a) = a
    vadd x y = fill_by (rows x) (cols x) (\i j -> Field.fadd (fromJust(idx x i j)) (fromJust(idx y i j)))
    vneg = smap Field.fneg
    fmul x c = smap (Field.fmul c) x


main = do
    let m = Matrix 3 3 [[1, 0, 2], [1, 2, 3], [1, 4, 6]] :: Matrix Double
    let m2 = smap (\x -> (x, x)) m
    print (mmul (smap (Field.fmul (2.0, 3.0)) m2) (vadd (id_mat 3) (transpose (id_mat 3)) :: Matrix (Double, Double)))
