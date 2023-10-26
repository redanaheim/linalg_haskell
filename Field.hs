module Field where
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
    fneg x = -1.0 * x

tmap :: (a -> a) -> (a, a) -> (a, a)
tmap f = (\x -> (f (fst x), f (snd x)))
tdmap :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
tdmap f = (\x y -> (f (fst x) (fst y), f (snd x) (snd y)))

instance (Field a) => Field (a, a) where
  fadd :: Field a => (a, a) -> (a, a) -> (a, a)
  fadd = tdmap fadd
  fmul = tdmap fmul
  fminv = tmap fminv
  fneg = tmap fneg  
  zero = (Field.zero, Field.zero)
  one = (Field.one, Field.one)
-- 0 = false
-- 1 = true
-- fmul a b = xor
instance Field Bool where
    zero = False
    one = True

    fmul = (&&)
    fadd = (\x y -> not (x == y))
    fminv = not
    fneg = id