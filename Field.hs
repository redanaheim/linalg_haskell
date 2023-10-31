{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
module Field where
import Data.Ratio

class (Eq a, Show a) => Field a where
    fadd, fmul :: a -> a -> a
    fminv, fneg :: a -> a
    zero :: a
    one :: a

instance Field Double where
    zero = 0.0
    one = 1.0
    fadd x y = x + y
    fmul :: Double -> Double -> Double
    fmul x y = x * y
    fminv :: Double -> Double
    fminv x = 1.0 / x
    fneg x = x * (-1.0)

tmap :: (a -> a) -> (a, a) -> (a, a)
tmap f x = (f (fst x), f (snd x))
tdmap :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
tdmap f x y = (f (fst x) (fst y), f (snd x) (snd y))

instance (Field a) => Field (a, a) where
  fadd :: Field a => (a, a) -> (a, a) -> (a, a)
  fadd = tdmap fadd
  fmul = tdmap fmul
  fminv = tmap fminv
  fneg = tmap fneg
  zero = (Field.zero, Field.zero)
  one = (Field.one, Field.one)

slow_int_mul :: (Field a) => Int -> a -> a
slow_int_mul c x
    | c < 0 = Field.fadd (Field.fneg x) (slow_int_mul (c + 1) x)
    | c == 0 = Field.zero
    | otherwise = Field.fadd x (slow_int_mul (c - 1) x)

slow_exp :: (Field a) => a -> Int -> a
slow_exp base pow 
    | pow < 0 = Field.fmul (Field.fminv base) (slow_exp base (pow + 1))
    | pow == 0 = Field.one
    | pow > 0 = Field.fmul base (slow_exp base (pow - 1))

-- 0 = false
-- 1 = true
-- fmul a b = xor
instance Field Bool where
    zero = False
    one = True

    fmul = (&&)
    fadd = (/=)
    fminv = not
    fneg = id

instance Field (Ratio Integer) where
    fadd x y = do
        let denom = lcm (denominator x) (denominator y)
        ((numerator x * quot denom (denominator x)) + (numerator y * (quot denom (denominator y) :: Integer))) % denom

    fmul x y = (numerator x * numerator y) % (denominator x * denominator y)
    fminv x = denominator x % numerator x
    fneg x = (-1 * numerator x) % denominator x
    zero = 0 % 1
    one = 1 % 1
