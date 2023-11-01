{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expression where
import Field
import Data.Ratio

data ComputationContext where
    ComputationContext :: { value :: String -> Double } -> ComputationContext

data BinaryOp where
    BinaryOp :: { bi_compute_val :: ComputationContext -> Expression -> Expression -> (ComputationContext, Double), bi_show :: Expression -> Expression -> String } -> BinaryOp

data UnaryOp where
    UnaryOp :: { un_compute_val :: ComputationContext -> Expression -> (ComputationContext, Double), un_show :: Expression -> String } -> UnaryOp

data Expression where
    Binary :: { l :: Expression, r :: Expression, bi_op :: BinaryOp } -> Expression
    Unary :: { x :: Expression, un_op :: UnaryOp } -> Expression
    Variable :: { name :: String } -> Expression
    RationalConst :: { rational_value :: Ratio Integer } -> Expression

compute :: ComputationContext -> Expression -> Double
compute cc xpr = case xpr of
    Binary l r op -> snd (bi_compute_val op cc l r)
    Unary x op -> snd (un_compute_val op cc x)
    Variable name -> value cc name
    RationalConst x -> fromIntegral (numerator x) / fromIntegral (denominator x)

add_op :: BinaryOp
add_op = BinaryOp (\cc x y -> (cc, compute cc x + compute cc y)) (\x y -> "(" ++ show x ++ " + " ++ show y ++ ")") -- "add"

sub_op :: BinaryOp
sub_op = BinaryOp (\cc x y -> (cc, compute cc x - compute cc y)) (\x y -> "(" ++ show x ++ " - " ++ show y ++ ")") -- "sub"

mul_op :: BinaryOp
mul_op = BinaryOp (\cc x y -> (cc, compute cc x * compute cc y)) (\x y -> "(" ++ show x ++ show y ++ ")") -- "mul"

div_op :: BinaryOp
div_op = BinaryOp (\cc x y -> (cc, compute cc x / compute cc y)) (\x y -> "(" ++ show x ++ "/" ++ show y ++ ")") -- "div"

sqrt_op :: UnaryOp
sqrt_op = UnaryOp (\cc x -> (cc, sqrt (compute cc x))) (\x -> "sqrt(" ++ show x ++ ")") -- "sqrt"

exp_op :: UnaryOp
exp_op = UnaryOp (\cc x -> (cc, euler ** compute cc x)) (\x -> "exp(" ++ show x ++ ")") -- "exp"

log_op :: UnaryOp
log_op = UnaryOp (\cc x -> (cc, Prelude.log (compute cc x))) (\x -> "ln(" ++ show x ++ ")") -- "log"

xpr_add x y = Binary x y add_op
xpr_sub x y = Binary x y sub_op
xpr_mul x y = Binary x y mul_op
xpr_div x y = Binary x y div_op
xpr_sqrt x = Unary x sqrt_op
xpr_exp x = Unary x exp_op
xpr_log x = Unary x log_op

instance Show Expression where
    show x = case x of
        Binary l r op -> bi_show op l r
        Unary x op -> un_show op x
        Variable name -> "(" ++ name ++ ")"
        RationalConst x -> "(" ++ show x ++ ")"

instance Eq Expression where
    x == y = show x == show y

instance Field Expression where
    zero = RationalConst 0
    one = RationalConst 1

    fadd = xpr_add
    fmul = xpr_mul
    fminv = xpr_div Field.one
    fneg = fmul (RationalConst (-1))

instance RealField Expression where
    exp = xpr_exp
    log = xpr_log
    midpoint x y = xpr_div (fadd x y) (RationalConst 2)

main = do
    let x = Variable "x"
    let xpr = xpr_div (xpr_add (xpr_sqrt x) (Variable "y")) (RationalConst (2 % 5))
    print (compute (ComputationContext (const 2)) xpr)
    print (show xpr)
