This is a library for handling type-safe multidimensional matrices. 

Some key features include that it has value-level indices, rather than only type-level
every dimension must be greater than zero
it provides type-safe lenses and isomorphisms for accessing a matrix
read and show instances have special handling of one-dimensional (Vec) and two-dimensional (Mat2) matrices
it provides the ability to converters between nested lists and matrices
as well as between nested tuples and matrices

There are three main types on which the library is based:

1.       Fin: is an index for a single column/row
2.       FinMat: is an index for a matrix
3.       Mat: is a multidimensional matrix

Among the instances that Mat supports are:

*         Read
*         Show
*         Enum
*         Num
*         Num1  (similar to the Num class but safely respects bounds)
*         Functor
*         Applicative
*         Monad
*         Representable
*         Traversable
*         Foldable
*         FunctorWithIndex
*         FoldableWithIndex
*         TraversableWithIndex
*         Traversable1
*         Foldable1

This library has methods using type-level indices, and has methods using value-level Fin and FinMat indices. 

Some additional functionality includes matrix multiplication / transpose / subset slicing / matrix leaf traversals. 

How to create a matrix:

mat @'[2,3,4,5] [1..] creates a 2x3x4x5 matrix containing 1..120 (2*3*4*5)

Note that vec @5 [1..] is a shortcut for mat @'[5] [1..]

And mat2 @4 @7 [1..] is just a shortcut for mat @'[4,7] [1..]

mat @'[3,4] [1..] * 4 
