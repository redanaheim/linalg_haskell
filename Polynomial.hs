{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Vector
import Matrix
import Solvable
import Util
import Field
import Data.Ratio (Ratio)

data Polynomial a where
    Polynomial :: (Field a) => { coefs :: [a] } -> Polynomial a

instance (Field a) => Eq (Polynomial a) where
    x == y = coefs x == coefs y

instance (Field a) => Vector (Polynomial a) where
    type Scalar (Polynomial a) = a

    vadd x y = Polynomial (Vector.vadd (coefs x) (coefs y))
    vneg x = Polynomial (fmap fneg (coefs x))
    fmul x c = Polynomial (fmap (Field.fmul c) (coefs x))

concat_with :: [String] -> String -> String
concat_with x separator
    | length x == 1 = head x
    | otherwise = head x ++ separator ++ concat_with (tail x) separator


instance (Field a) => Show (Polynomial a) where
    show x = concat_with (ifmap (\c i -> show c ++ "x^" ++ show i) (coefs x)) " + "

instance (Field a) => Solvable (Polynomial a) where
  get_coord basis x = get_coord (fmap coefs basis) (coefs x)

derivative :: (Field a) => Polynomial a -> Polynomial a
derivative p = Polynomial ([slow_int_mul i (fidx (i + 1) (coefs p)) | i <- [1..(length (coefs p) - 1)]] ++ [Field.zero])

evaluate :: (Field a) => Polynomial a -> a -> a
evaluate p x = foldr Field.fadd Field.zero (ifmap (\c i -> Field.fmul c (slow_exp x (i - 1))) (coefs p))

polynomial_std_basis_component :: (Field a) => Int -> Int -> Polynomial a
polynomial_std_basis_component i l = Polynomial (take l (concat [replicate (i - 1) Field.zero, [Field.one], repeat Field.zero]))

polynomial_std_basis :: (Field a) => Int -> [Polynomial a]
polynomial_std_basis l = [ polynomial_std_basis_component i l | i <- [1..l]]

constant :: (Field a) => Int -> a -> Polynomial a
constant l c = Polynomial (take l (c : repeat Field.zero))

main = do
    let std = polynomial_std_basis 5 :: [Polynomial (Ratio Integer)]
    let p = Polynomial [1, 2, 3, 4, 5] :: Polynomial (Ratio Integer)
    let basis = [Polynomial [1, 1, 1, 1, 1], Polynomial [1, 0, 1, 0, 1], Polynomial [1, 2, 3, 4, 5], Polynomial [1, 0, 0, 0, 0], Polynomial [0, 1, 0, 0, 0]] :: [Polynomial (Ratio Integer)]
    let d = LinearTransformation (\val -> Vector.vadd (derivative val) (constant 5 (evaluate val 1)))
    print (slow_exp (2 :: (Ratio Integer)) 2)
    print ((ifmap (\c i -> Field.fmul c (slow_exp 2 i)) . coefs) (head basis))
    print (evaluate (head basis) 2)
    print (get_matrix basis basis d)