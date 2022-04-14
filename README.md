# cybus: a library for handling type-safe multidimensional matrices. 

[![Hackage](https://img.shields.io/hackage/v/cybus.svg?colorB=5d0ef0&style=flat)](https://hackage.haskell.org/package/cybus)

Some key features include that it has value-level indices, rather than only type-level
every dimension must be greater than zero
it provides type-safe lenses and isomorphisms for accessing a matrix
read and show instances have special handling for one-dimensional (Vec) and two-dimensional (Mat2) matrices
it provides the ability to converters between nested lists and matrices
as well as between nested tuples and matrices

There are three main types used by this library:

1. Fin: is an index for a single column/row
2. FinMat: is an index for a matrix
3. Mat: is a multidimensional matrix

Among the instances that Mat supports are:
* Read
* Show
* Enum
* Num
* Num1  (similar to the Num class but safely respects bounds)
* Functor
* Applicative
* Monad
* Representable
* Traversable
* Foldable
* FunctorWithIndex
* FoldableWithIndex
* TraversableWithIndex
* Traversable1
* Foldable1

This library has methods using type-level indices, and has methods using value-level Fin and FinMat indices. 

Some additional functionality includes matrix multiplication / transpose / subset slicing / matrix leaf traversals. 

# How to create a matrix

mat @'[2,3,4,5] [1..] creates a 2x3x4x5 matrix containing 1..120 (2*3*4*5)

```haskell
>mat @'[3,5,4] [1..]
Mat@[3,5,4]
  [
     [
        [1,2,3,4],
        [5,6,7,8],
        [9,10,11,12],
        [13,14,15,16],
        [17,18,19,20]
     ],[
        [21,22,23,24],
        [25,26,27,28],
        [29,30,31,32],
        [33,34,35,36],
        [37,38,39,40]
     ],[
        [41,42,43,44],
        [45,46,47,48],
        [49,50,51,52],
        [53,54,55,56],
        [57,58,59,60]
     ]
  ]
```

```haskell
>mat @'[3,4] [1..] * 4
Mat2@(3,4)
  [
     [4,8,12,16],
     [20,24,28,32],
     [36,40,44,48]
  ]
```

# Lenses for matrices

```haskell
>mat @'[3,4,3] [1..] ^. headMat
Mat2@(4,3)
  [
     [1,2,3],
     [4,5,6],
     [7,8,9],
     [10,11,12]
  ]
```

```haskell
>mat @'[3,4,3] [1..] ^. _r3
Mat2@(4,3)
  [
     [25,26,27],
     [28,29,30],
     [31,32,33],
     [34,35,36]
  ]
```

```haskell
>mat @'[3,4,3] [1..] ^. _c2
Mat2@(3,3)
  [
     [4,5,6],
     [16,17,18],
     [28,29,30]
  ]
```

```haskell
>mat @'[3,4,3] [1..] ^. _c2 . _transposeMat . consMat . _2
Mat2@(2,3)
  [
     [5,17,29],
     [6,18,30]
  ]
```

# matrix multiplication 

```haskell
>multMat (mat2 @4 @3 [1..]) (mat2 @3 @2 [10..])
Mat2@(4,2)
  [
     [76,82],
     [184,199],
     [292,316],
     [400,433]
  ]
```

# slice a matrix 

```haskell
>sliceC @'[3, 5] @'[4, 6, 2] (mat [1..])
Vec@2 [33,34]
```

```haskell
>sliceC @'[3, 5] @'[4, 6, 2] (mat [1..])
Vec@2 [33,34]
```

```haskell
>sliceC @'[3, 5, 2] @'[4, 6, 2] (mat [1..])
34
```



