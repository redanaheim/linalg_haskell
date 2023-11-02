{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Expression where
import Field
import Data.Ratio
import Polynomial (concat_with)

data ComputationContext where
    ComputationContext :: { value :: String -> Double } -> ComputationContext

data ListOp where
    ListOp :: { list_compute_val :: ComputationContext -> [Expression] -> (ComputationContext, Double), list_show :: [Expression] -> String, list_op_name :: String } -> ListOp

data BinaryOp where
    BinaryOp :: { bi_compute_val :: ComputationContext -> Expression -> Expression -> (ComputationContext, Double), bi_show :: Expression -> Expression -> String, bi_op_name :: String } -> BinaryOp

data UnaryOp where
    UnaryOp :: { un_compute_val :: ComputationContext -> Expression -> (ComputationContext, Double), un_show :: Expression -> String, un_op_name :: String } -> UnaryOp

data Expression where
    List :: { subjects :: [Expression], list_op :: ListOp } -> Expression
    Binary :: { l :: Expression, r :: Expression, bi_op :: BinaryOp } -> Expression
    Unary :: { un_content :: Expression, un_op :: UnaryOp } -> Expression
    Variable :: { name :: String } -> Expression
    RationalConst :: { rational_value :: Ratio Integer } -> Expression

compute :: ComputationContext -> Expression -> Double
compute cc xpr = case xpr of
    List s op -> snd (list_compute_val op cc s)
    Binary l r op -> snd (bi_compute_val op cc l r)
    Unary x op -> snd (un_compute_val op cc x)
    Variable name -> value cc name
    RationalConst x -> fromIntegral (numerator x) / fromIntegral (denominator x)

add_op :: ListOp
add_op = ListOp (\cc s -> (cc, foldr ((+) . compute cc) 0 s)) (\s -> concat_with (fmap show s) " + ")  "add"

sub_op :: BinaryOp
sub_op = BinaryOp (\cc x y -> (cc, compute cc x - compute cc y)) (\x y -> "(" ++ show x ++ " - " ++ show y ++ ")") "sub"

mul_op :: ListOp
mul_op = ListOp (\cc s -> (cc, foldr ((*) . compute cc) 1 s)) (\s -> concat_with (fmap show s) " * ") "mul"

div_op :: BinaryOp
div_op = BinaryOp (\cc x y -> (cc, compute cc x / compute cc y)) (\x y -> "(" ++ show x ++ "/" ++ show y ++ ")") "div"

sqrt_op :: UnaryOp
sqrt_op = UnaryOp (\cc x -> (cc, sqrt (compute cc x))) (\x -> "sqrt(" ++ show x ++ ")") "sqrt"

exp_op :: UnaryOp
exp_op = UnaryOp (\cc x -> (cc, euler ** compute cc x)) (\x -> "exp(" ++ show x ++ ")") "exp"

log_op :: UnaryOp
log_op = UnaryOp (\cc x -> (cc, Prelude.log (compute cc x))) (\x -> "ln(" ++ show x ++ ")") "log"

neg_op :: UnaryOp
neg_op = UnaryOp (\cc x -> (cc, (-1) * compute cc x)) (\x -> "-(" ++ show x ++ ")") "neg"

minv_op :: UnaryOp
minv_op = UnaryOp (\cc x -> (cc, fminv (compute cc x))) (\x -> "((" ++ show x ++ ")^(-1))") "minv"

xpr_add x y = List [x, y] add_op
xpr_sub x y = Binary x y sub_op
xpr_mul x y = List [x, y] mul_op
xpr_div x y = Binary x y div_op
xpr_sqrt x = Unary x sqrt_op
xpr_exp x = Unary x exp_op
xpr_log x = Unary x log_op
xpr_neg x = Unary x neg_op
xpr_minv x = Unary x minv_op

instance Show Expression where
    show x = case x of
        List s op -> list_show op s
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
    fminv = xpr_minv
    fneg = xpr_neg

instance RealField Expression where
    exp = xpr_exp
    log = xpr_log
    midpoint x y = xpr_div (fadd x y) (RationalConst 2)

is_const :: Expression -> Bool
is_const (RationalConst _) = True
is_const _ = False

partial_simplify_add_op :: [Expression] -> [Expression]
partial_simplify_add_op [] = []
partial_simplify_add_op (h:rest) = do
    let xpr = simplify h
    if xpr == RationalConst 0 then partial_simplify_add_op rest else do
        case xpr of
            List s op -> if list_op_name op == "add" then partial_simplify_add_op (rest ++ s) else xpr:partial_simplify_add_op rest
            Binary l r op -> if bi_op_name op == "sub" then partial_simplify_add_op (rest ++ [l, fneg r]) else xpr:partial_simplify_add_op rest
            Unary x op -> xpr:partial_simplify_add_op rest
            RationalConst x -> (partial_simplify_add_op (filter (not . is_const) rest)) ++ [RationalConst (foldr ((\x t -> t + rational_value x) . simplify) x (filter is_const rest))]

simplify_add_op :: [Expression] -> Expression
simplify_add_op [xpr] = simplify xpr
simplify_add_op s = do
    let partial = partial_simplify_add_op s
    if null partial then RationalConst 0 else List partial add_op

partial_simplify_mul_op :: [Expression] -> [Expression]
partial_simplify_mul_op [] = []
partial_simplify_mul_op (h:rest) = do
    let xpr = simplify h
    if xpr == RationalConst 1 then partial_simplify_mul_op rest else do
        case xpr of
            List s op -> if list_op_name op == "mul" then partial_simplify_mul_op (rest ++ s) else xpr:partial_simplify_mul_op rest
            Binary l r op -> if bi_op_name op == "div" then partial_simplify_add_op (rest ++ [l, fminv r]) else xpr:partial_simplify_mul_op rest
            Unary x op -> xpr:partial_simplify_mul_op rest
            RationalConst x -> (partial_simplify_mul_op (filter (not . is_const) rest)) ++ [RationalConst (foldr ((\x t -> t * rational_value x) . simplify) x (filter is_const rest))]

simplify_mul_op :: [Expression] -> Expression
simplify_mul_op [xpr] = simplify xpr
simplify_mul_op s = do
    let partial = partial_simplify_mul_op s
    if null partial then RationalConst 1 else do
        if RationalConst 0 `elem` partial then RationalConst 0 else List partial add_op

simplify_list_op :: [Expression] -> ListOp -> Expression
simplify_list_op s op = case list_op_name op of
    "add" -> simplify_add_op s
    "mul" -> simplify_mul_op s
    otherwise -> List s op

simplify_binary_op :: Expression -> Expression -> BinaryOp -> Expression
simplify_binary_op l r op = case bi_op_name op of
    "sub" -> simplify_add_op [l, fneg r]
    "div" -> simplify_mul_op [l, fminv r]
    otherwise -> Binary l r op

is_unary_op :: UnaryOp -> Expression -> Bool
is_unary_op op (Unary _ other_op) = un_op_name op == un_op_name other_op
is_unary_op op _ = False

simplify_unary_op :: Expression -> UnaryOp -> Expression
simplify_unary_op x op = case un_op_name op of
    "neg" -> if is_unary_op neg_op x then simplify (un_content x) else do
        if is_const x then RationalConst (fneg (rational_value x)) else Unary (simplify x) op
    otherwise -> Unary (simplify x) op

simplify :: Expression -> Expression
simplify x = case x of
    List s op -> simplify_list_op s op
    Binary l r op -> simplify_binary_op l r op
    Unary x op -> simplify_unary_op x op
    RationalConst x -> RationalConst x

