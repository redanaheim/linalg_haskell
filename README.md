# linalg_haskell

Linear algebra library written in Haskell.

The aim of the library is to be able to represent and deal with all vector spaces with enumerable bases.
For example, operations on the set of all polynomial functions as a vector space should be supported.

## Library

### `Field` 
```hs
class (Eq a, Show a) => Field a where 
    fadd, fmul :: a -> a -> a -- add or multiply two elements of the field
    fminv, fneg :: a -> a -- fminv: returns the multiplicative inverse of input, fneg: returns the additive inverse
    zero :: a -- additive identity of the field
    one :: a -- multiplicative identity of the field
```

A type that instantiates `Field` must satisfy the [field axioms](https://mathworld.wolfram.com/FieldAxioms.html), otherwise behavior is undefined.

### `Vector`
```hs
class (Eq a, Field (Scalar a)) => Vector a where
    type Scalar a :: Type
    vadd :: a -> a -> a -- vector addition
    vneg :: a -> a -- additive inverse of a vector

    fmul :: a -> Scalar a -> a -- scalar multiplication
```

A type that instantiates `Vector` must satisfy the [vector space axioms](https://math.colorado.edu/~jonathan.wise/teaching/math3135-spring-2017/exp01.pdf), otherwise behavior is undefined.

### `InnerProduct`
```hs
class (Vector a) => InnerProduct a where
    inner :: a -> a -> Scalar a 
```

A type that instantiates `InnerProduct` must satisfy the [inner product space axioms](https://www.math.ucdavis.edu/~anne/WQ2007/mat67-Lj-Inner_Product_Spaces.pdf), otherwise behavior is undefined.

### `Matrix`
```hs
data Matrix a where
    Matrix :: (Field a) => {
        rows :: Int,
        cols :: Int,
        content :: [[a]]
    } -> Matrix a

idx :: Matrix a -> Int -> Int -> Maybe a
row :: Matrix a -> Int -> Maybe [a]
col :: Matrix a -> Int -> Maybe [a]

-- fill_by rows cols f generates a matrix of dimensions rows x cols by calling f with indices i,j for each spot in the matrix, with i <- 1..<rows and j <- 1..<cols
fill_by :: (Field a) => Int -> Int -> (Int -> Int -> a) -> Matrix a

mmul :: Field a => Matrix a -> Matrix a -> Matrix a
```

Matrix instantiates `Vector`.

## Current Goals (in no particular order)

1. Matrix multiplication (done)
2. Reduced row-echelon form (done)
3. Represent linear transformations on arbitrary vector spaces with matrices (done!)
4. Get bases for the image and kernel of arbitrary linear transformations (done!)
5. Represent the vector space of all polynomial functions
6. Get change-of-basis matrices
7. Gram-Schmidt process
8. Get the orthogonal complement of a subspace spanned by some set of vectors
9. Safety on creating matrices of the wrong size
