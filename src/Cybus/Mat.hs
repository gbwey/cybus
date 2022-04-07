{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Cybus.Mat
Description : type level indexed multi-dimensional matrix
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Cybus.Mat (
  type Mat,
  mVec,
  mIndices,
  pattern Mat,
  pattern MatU,
  Vec,
  Mat2,
  Mat3,
  Mat4,
  Mat5,
  Mat6,
  MatN,

  -- * cons/snoc lenses
  ConsMatC (..),
  SnocMatC (..),
  Eof1 (..),
  EofN (..),

  -- * tuple conversions
  MatTupleC (..),
  MatTupleT,
  ListTupleCInternal (..),

  -- * converters
  MatConvertersC (..),
  nestedListToMatValidated,
  nestedNonEmptyToMatValidated,
  MatToNestedVecT,

  -- * bulk construct matrix
  mkMat,
  mkMatC,
  mat,
  mat',
  vec,
  vec',
  mat2,
  mat2',
  gen',
  gen,
  mm,
  buildMat,

  -- * vector/matrix builders
  (.:),
  se1,
  (.::),
  se2,
  (.|),
  (.||),

  -- * indexing
  ixMat,
  ixMat',
  setMat,
  updateMat,
  indexMat,
  finMatRows,

  -- * reverse
  reverseRows,

  -- * sort
  sortByRows,
  multMat,
  -- dot,
  DotC (..),

  -- * zip
  zipWithMat,
  zipWithMat3,
  zipMat,
  zipWithMatA,
  izipWith,
  izipWithM,

  -- * general
  cartesian,
  pureMat,
  replicateMat,

  -- * row operations
  deleteRow,
  deleteRow',
  insertRow,
  insertRow',
  swapRow,
  swapRow',
  _row,
  _row',
  rows,
  unrows,
  _rows,
  wrapRows1,
  indexRow,

  -- * column operations
  deleteCol,
  deleteCol',
  insertCol,
  insertCol',
  swapCol,
  swapCol',
  _col,
  _col',
  swapMat,
  swapMat',
  appendV,
  appendH,
  permutationsMat,
  findMatElems,

  -- * bulk updates
  bulkMat,
  updatesMat,
  getsMat,
  setsMat,
  nonEmptyMatsToMat,

  -- * reshape
  _transposeMat,
  transposeMat,
  toND,
  MatToNDT,
  toVec,
  toMat2,
  toMat3,
  concatMat,
  redim,
  reverseDim,

  -- * subset and slicing
  SliceC (..),
  SliceT,
  SliceC' (..),
  SliceT',
  slice,
  sliceUpdate,
  sliceToFinMat,
  SliceToFinMatT,
  ixSlice,
  ixSlice',
  subsetRows,
  subsetCols,
  diagonal,
  rowsToMat,

  -- * splitting
  chunkNV,
  chunkNVMat,

  -- * leaf methods
  LeafC (..),
  traverseLeafSimple,
  mapLeafSimple,
  foldMapLeaf,
  foldMapLeafR,
  mapLeaf,
  mapLeafS,
  mapLeafSimpleS,
  foldLeaf,
  toLeaves,
  mapCols,
  mapCols',

  -- * read/show/print methods
  ShowMatC (..),
  ShowOpts (..),
  defShowOpts,
  prtMat,
  showMat,
  readMatP,
  readMat,
  readMat2,
  readVec,

  -- * row lenses
  Row1 (..),
  Row2 (..),
  Row3 (..),
  Row4 (..),
  Row5 (..),
  Row6 (..),
  Row7 (..),
  Row8 (..),
  Row9 (..),
  Row10 (..),

  -- * column lenses
  _c1,
  _c2,
  _c3,
  _c4,
  _c5,
  _c6,
  _c7,
  _c8,
  _c9,
  _c10,

  -- * miscellaneous
  finMatMatrix,
  finMatMatrix',

  -- * scans
  scanlVec,
  scanrVec,
  postscanlMat,
  postscanrMat,

  -- ** coercion methods to set the dimensions of a matrix
  dim1,
  dim2,
  dim3,
  dim4,
  dim5,
  dim6,
  dim7,
  dim8,
  dim9,
  dim10,
) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import qualified Control.Monad.State.Strict as S
import Control.Monad.Zip
import Cybus.Fin
import Cybus.FinMat
import Cybus.NatHelper
import Data.Bool
import Data.Coerce
import Data.Distributive
import Data.Foldable
import Data.Foldable.WithIndex
import qualified Data.Functor.Apply as Apply
import qualified Data.Functor.Bind as Bind
import Data.Functor.Identity
import Data.Functor.Rep
import Data.Functor.WithIndex
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.String
import Data.Traversable.WithIndex
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Enum
import qualified GHC.Exts as GE (IsList (..))
import GHC.Generics (Generic, Generic1)
import qualified GHC.Read as GR
import GHC.Stack
import qualified GHC.TypeLits as GL
import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as GN
import Primus.Enum
import Primus.Error
import Primus.Extra
import Primus.Fold
import Primus.Lens
import Primus.NonEmpty
import Primus.Num1
import Primus.One
import Primus.Rep
import qualified Primus.TypeLevel as TP
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC

-- | definition of a matrix
type Mat :: [Nat] -> Type -> Type
data Mat ns a = MatUnsafe !(Vector a) !(NonEmpty Pos)
  deriving stock (Functor, Traversable, Foldable, Generic, Generic1, Eq, Ord)
  deriving anyclass (NFData, NFData1)

-- | accessor for the relative position within a matrix
mVec :: Mat ns a -> Vector a
mVec (MatUnsafe v _) = v

-- | accessor for the indices of a matrix
mIndices :: Mat ns a -> NonEmpty Pos
mIndices (MatUnsafe _ ns) = ns

-- | convenient type synonym for a 1d matrix
type Vec :: Nat -> Type -> Type
type Vec n = Mat '[n]

-- | convenient type synonym for a 2d matrix
type Mat2 :: Nat -> Nat -> Type -> Type
type Mat2 n m = Mat '[n, m]

-- | convenient type synonym for a 3d matrix
type Mat3 :: Nat -> Nat -> Nat -> Type -> Type
type Mat3 n m p = Mat '[n, m, p]

-- | convenient type synonym for a 4d matrix
type Mat4 :: Nat -> Nat -> Nat -> Nat -> Type -> Type
type Mat4 n m p q = Mat '[n, m, p, q]

-- | convenient type synonym for a 5d matrix
type Mat5 :: Nat -> Nat -> Nat -> Nat -> Nat -> Type -> Type
type Mat5 n m p q r = Mat '[n, m, p, q, r]

-- | convenient type synonym for a 6d matrix
type Mat6 :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Type -> Type
type Mat6 n m p q r s = Mat '[n, m, p, q, r, s]

-- | convenient type synonym for specifying the dimensions of a matrix using the 'NN' type family
type MatN :: Nat -> Type -> Type
type MatN n = Mat (NN n)

-- | readonly pattern synonym for a matrix
{-# COMPLETE Mat #-}

pattern Mat ::
  forall (ns :: [Nat]) a.
  Vector a ->
  NonEmpty Pos ->
  Mat ns a
pattern Mat v ps <- MatUnsafe v ps

{-# COMPLETE MatIU #-}

-- | bidirectional pattern synonym for simple validation of a matrix before construction
pattern MatIU ::
  forall (ns :: [Nat]) a.
  HasCallStack =>
  Vector a ->
  NonEmpty Pos ->
  Mat ns a
pattern MatIU v ps <-
  MatUnsafe v ps
  where
    MatIU = frp .@ mkMat -- dont change this: frp is needed

{-# COMPLETE MatU #-}

-- | bidirectional pattern synonym for validating a matrix before construction with 'NS' constraint for additional typelevel validation
pattern MatU ::
  forall (ns :: [Nat]) a.
  (NS ns, HasCallStack) =>
  Vector a ->
  NonEmpty Pos ->
  Mat ns a
pattern MatU v ps <-
  MatUnsafe v ps
  where
    MatU = frp .@ mkMatC -- dont change this: frp is needed

instance (Bounded a, Enum a) => Num1 (Mat ns a) where
  fromInteger1 = toEnumTraversable
  toInteger1 = fromEnumFoldable1 -- need this as Enum is only Int but containers can be larger ie Integer

instance (Enum a, Bounded a, NS ns) => Enum (Mat ns a) where
  toEnum = forceRight "Enum Mat:toEnum" . toEnumRep . toInteger
  fromEnum = forceRight "Enum Mat:fromEnum" . integerToIntSafe . fromEnumFoldable1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance (NS ns, Bounded a) => Bounded (Mat ns a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (c ~ Char, NS ns) => IsString (Mat ns c) where
  fromString = mat

-- | generate a 'Mat' using a list
mat, mat' :: forall ns a. (HasCallStack, NS ns) => [a] -> Mat ns a
mat = fr . matImpl False
mat' = fr . matImpl True

matImpl :: forall ns a. NS ns => Bool -> [a] -> Either String (Mat ns a)
matImpl b = \case
  [] -> Left "matImpl: no data"
  x : xs -> do
    let ns = fromNSP @ns
        n = productP ns
    (as, zs) <- splitAt1GE n (x :| xs)
    case (b, zs) of
      (True, _ : _) -> Left "matImpl: found extras"
      _o -> Right $ MatU (V.fromListN (unP n) (N.toList as)) ns

-- | used by 'pure' so dont call pure from here
pureMat :: forall ns a. NS ns => a -> Mat ns a
pureMat a =
  let ns = fromNSP @ns
   in MatU (V.replicate (productPInt ns) a) ns

-- | creates a matrix of first dimension "n" by replicating the input matrix "n" times
replicateMat :: forall n n1 ns a. PosT n => Mat (n1 ': ns) a -> Mat (n ': n1 ': ns) a
replicateMat (Mat v ns) =
  let n = fromNP @n
   in MatIU (V.concat (replicate (unP n) v)) (n N.<| ns)

instance (NS ns, Num a) => Num (Mat ns a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger
  abs = fmap abs

instance (NS ns, Fractional a) => Fractional (Mat ns a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance NS ns => Applicative (Mat ns) where
  pure = pureMat
  (<*>) = ap2

ap2 :: Mat ns (a -> b) -> Mat ns a -> Mat ns b
ap2 (Mat vab ps) (Mat va _) = MatIU (V.zipWith id vab va) ps -- ziplist style

ap3 :: NS ns => (a -> Mat ns b) -> Mat ns a -> Mat ns b
ap3 f = imap (\fn -> indexMat fn . f)

instance Apply.Apply (Mat ns) where
  (<.>) = ap2

instance NS ns => Monad (Mat ns) where
  (>>=) = flip ap3

instance NS ns => Bind.Bind (Mat ns) where
  (>>-) = flip ap3

instance NS ns => MonadZip (Mat ns) where
  mzipWith = zipWithMat

-- | zip two matrices using a combining function
zipWithMat :: (a -> b -> c) -> Mat ns a -> Mat ns b -> Mat ns c
zipWithMat f (Mat v ps) (Mat w _) = MatIU (V.zipWith f v w) ps

-- | zip three matrices using a combining function
zipWithMat3 :: (a -> b -> c -> d) -> Mat ns a -> Mat ns b -> Mat ns c -> Mat ns d
zipWithMat3 f (Mat v ps) (Mat w _) (Mat x _) = MatIU (V.zipWith3 f v w x) ps

-- | zip two matrices
zipMat :: Mat ns a -> Mat ns b -> Mat ns (a, b)
zipMat = zipWithMat (,)

-- | 'zipWithMat' with an Applicative or use 'Primus.Fold.zipWithT' but that needs a 'NS' constraint
zipWithMatA ::
  Applicative f =>
  (a -> b -> f c) ->
  Mat ns a ->
  Mat ns b ->
  f (Mat ns c)
zipWithMatA f = traverse (uncurry f) .@ zipMat

-- | 'zipWithMat' with an index or use 'Primus.Rep.izipWithR'
izipWith ::
  NS ns =>
  (FinMat ns -> a -> b -> c) ->
  Mat ns a ->
  Mat ns b ->
  Mat ns c
izipWith f = zipWithMat3 f finMatMatrix

-- | 'zipWithMatA' with an index or use 'Primus.Rep.izipWithR' if "f" is 'Data.Distributive.Distributive'
izipWithM ::
  (NS ns, Applicative f) =>
  (FinMat ns -> a -> b -> f c) ->
  Mat ns a ->
  Mat ns b ->
  f (Mat ns c)
izipWithM f = itraverse (uncurry . f) .@ zipMat

instance Foldable1 (Mat ns) where
  foldMap1 f = foldMap1 f . nep . V.toList . mVec -- cant be empty (dont use 'toNonEmpty')

instance Traversable1 (Mat ns) where
  traverse1 f (Mat v ps) =
    case V.toList v of
      [] -> programmError "Mat: traverse1: empty vector"
      a : as -> (\(b :| bs) -> MatIU (V.fromList (b : bs)) ps) <$> traverse1 f (a :| as)

instance Semigroup a => Semigroup (Mat ns a) where
  (<>) = zipWithMat (<>)
instance (Monoid a, NS ns) => Monoid (Mat ns a) where
  mempty = pure mempty

instance NS ns => FunctorWithIndex (FinMat ns) (Mat ns) where
  imap f = snd . L.mapAccumL (\i a -> (i + 1, f (FinMatU i (fromNSP @ns)) a)) 0

instance NS ns => FoldableWithIndex (FinMat ns) (Mat ns) where
  ifoldMap f = fold . imap f

-- todo: write a dedicated version
instance NS ns => TraversableWithIndex (FinMat ns) (Mat ns) where
  itraverse f = sequenceA . imap f

instance NS ns => Distributive (Mat ns) where
  collect agb fa =
    let z = agb <$> fa
     in imap (\fm -> const ((V.! fmPos fm) . mVec <$> z)) (pure ())

-- | index into a matrix
indexMat :: FinMat ns -> Mat ns a -> a
indexMat fm = (V.! fmPos fm) . mVec

-- | create a matrix of matrix indices for a given size "ns"
finMatMatrix :: forall ns. NS ns => Mat ns (FinMat ns)
finMatMatrix = finMatMatrix' (pure ())

-- | fill an existing matrix with indices
finMatMatrix' :: forall ns x. NS ns => Mat ns x -> Mat ns (FinMat ns)
finMatMatrix' = imap const

instance NS ns => Representable (Mat ns) where
  type Rep (Mat ns) = FinMat ns
  tabulate f = imap (const . f) (pure ())
  index = flip indexMat

instance NS ns => GE.IsList (Mat ns a) where
  type Item (Mat ns a) = a
  fromList = snd . fr . fillTraversable (pure ())
  toList = toListMat

-- | validate before creating a matrix
mkMat :: forall ns a. Vector a -> NonEmpty Pos -> Either String (Mat ns a)
mkMat v ps =
  let n1 = productPInt ps
      n2 = V.length v
      ret = n1 == n2
   in if ret
        then Right (MatUnsafe v ps)
        else Left $ "\n\nproduct of " ++ show (fromPositives ps) ++ "=" ++ show n1 ++ "\nvector length=" ++ show n2 ++ "\n"

-- | validate before creating a matrix with extra 'NS' constraint to check that "ns" and 'mIndices' match
mkMatC :: forall ns a. NS ns => Vector a -> NonEmpty Pos -> Either String (Mat ns a)
mkMatC v ps = do
  let ps1 = fromNSP @ns
  if ps == ps1
    then mkMat v ps
    else Left $ "\nns mismatch: expected: " ++ show (fromPositives ps1) ++ " but found " ++ show (fromPositives ps)

-- | generate a matrix passing the indices at that element to a user callback function
gen' :: forall ns a. NS ns => ([Int] -> a) -> Mat ns a
gen' f = tabulate (f . fromPositives . finMatToNonEmpty @ns)

-- | generate a matrix passing a relative position of the element to a user callback function
gen :: forall ns a. NS ns => (Int -> a) -> Mat ns a
gen f = tabulate (f . fmPos)

-- | generate a matrix using relative position starting at one
mm :: forall ns. NS ns => Mat ns Int
mm = gen (+ 1)

-- | lens that accesses a value inside a mat given a concrete mat index
ixMat :: forall (ns :: [Nat]) a. FinMat ns -> Lens' (Mat ns a) a
ixMat i = lens (indexMat i) (\s b -> setMat b i s)

-- | lens that accesses a value inside a mat using a type level index
ixMat' ::
  forall (is :: [Nat]) (ns :: [Nat]) a.
  FinMatC is ns =>
  Lens' (Mat ns a) a
ixMat' = ixMat (finMatC @is @ns)

-- | sets a value in a matrix
setMat :: a -> FinMat ns -> Mat ns a -> Mat ns a
setMat a fm (Mat v ps) = MatIU (V.update v (V.singleton (fmPos fm, a))) ps

-- | updates a value in a matrix
updateMat :: (a -> a) -> FinMat ns -> Mat ns a -> Mat ns a
updateMat f (FinMat i _) (Mat v ps) =
  let (v1, v2) = V.splitAt i v
   in case V.uncons v2 of
        Just (a, v2') -> MatIU (v1 <> V.cons (f a) v2') ps
        Nothing -> programmError $ "updateMat: i=" ++ show i

-- | cons a value with a 1d matrix
(.:) :: forall n a a'. a ~ a' => a -> Vec n a' -> Vec (1 GN.+ n) a'
a .: Mat v (p :| ps) = MatIU (V.cons a v) (succP p :| ps)

infixr 4 .:

-- | cons a matrix with a one-higher dimension matrix
(.::) :: forall n m ns a. Mat (m ': ns) a -> Mat (n ': m ': ns) a -> Mat (1 GN.+ n ': m ': ns) a
Mat v (_ :| _) .:: Mat v1 (p1 :| ps1) = MatIU (v <> v1) (succP p1 :| ps1)

infixr 3 .::

-- | combine two values together into 1d matrix
(.|) :: forall a a'. a ~ a' => a -> a' -> Vec 2 a'
a .| a' = MatU (V.cons a (V.singleton a')) (_2P :| [])

infixr 4 .|

-- | combine two matrices
(.||) :: forall m ns a. Mat (m ': ns) a -> Mat (m ': ns) a -> Mat (2 ': m ': ns) a
Mat v (_ :| _) .|| Mat v1 (p1 :| ps1) = MatIU (v <> v1) (_2P :| p1 : ps1)

infixr 3 .||

-- | last element in a 1d matrix
se1 :: forall a. a -> Vec 1 a
se1 a = MatU (V.singleton a) (_1P :| [])

-- | last element in a 2d or greater matrix
se2 :: forall n ns a. Mat (n ': ns) a -> Mat (1 ': n ': ns) a
se2 (Mat v ps) = MatIU v (_1P N.<| ps)

-- | create a 1d matrix from a list of values
vec :: forall n a. (HasCallStack, PosT n) => [a] -> Vec n a
vec = mat @'[n]

-- | create a 1d matrix from a list of values with the exact number of elements
vec' :: forall n a. (HasCallStack, PosT n) => [a] -> Vec n a
vec' = mat' @'[n]

-- | create a 2d matrix from a list of values
mat2 :: forall n m a. (HasCallStack, PosT n, PosT m) => [a] -> Mat2 n m a
mat2 = mat @'[n, m]

-- | create a 2d matrix from a list of values with the exact number of elements
mat2' :: forall n m a. (HasCallStack, PosT n, PosT m) => [a] -> Mat2 n m a
mat2' = mat' @'[n, m]

-- | map each column
mapCols ::
  forall n m ns a b.
  (FinMat (m ': n ': ns) -> Vec (TP.LastT (n ': ns)) a -> Vec (TP.LastT (n ': ns)) b) ->
  Mat (n ': m ': ns) a ->
  Mat (n ': m ': ns) b
mapCols f = transposeMat . mapLeafSimple f . transposeMat

-- | map each column with user state
mapCols' ::
  forall n m ns a b c.
  (FinMat (m ': n ': ns) -> c -> Vec (TP.LastT (n ': ns)) a -> (c, Vec (TP.LastT (n ': ns)) b)) ->
  c ->
  Mat (n ': m ': ns) a ->
  (c, Mat (n ': m ': ns) b)
mapCols' f c = fmap transposeMat . mapLeafSimpleS f c . transposeMat

-- | traverse over a nested leaf matrix only allowing changes to "a"
traverseLeafSimple ::
  (LeafC ns, Applicative m) =>
  (FinMat ns -> Vec (TP.LastT ns) a -> m (Vec (TP.LastT ns) b)) ->
  Mat ns a ->
  m (Mat ns b)
traverseLeafSimple f = fmap fromLeavesInternalC . traverseLeafC f

-- | map over a nested leaf matrix only allowing changes to "a"
mapLeafSimple ::
  LeafC ns =>
  (FinMat ns -> Vec (TP.LastT ns) a -> Vec (TP.LastT ns) b) ->
  Mat ns a ->
  Mat ns b
mapLeafSimple f = fromLeavesInternalC . runIdentity . traverseLeafC (Identity .@ f)

-- | foldmap over a nested leaf matrix
foldMapLeaf
  , foldMapLeafR ::
    (Monoid z, LeafC ns) =>
    (FinMat ns -> Vec (TP.LastT ns) a -> z) ->
    Mat ns a ->
    z
foldMapLeaf f = getConst . traverseLeafC (Const .@ f)
foldMapLeafR f = getConst . forwards . traverseLeafC ((Backwards . Const) .@ f)

-- | map over a nested leaf matrix
mapLeaf ::
  LeafC ns =>
  (FinMat ns -> Vec (TP.LastT ns) a -> b) ->
  Mat ns a ->
  Mat (TP.InitT ns) b
mapLeaf f = runIdentity . traverseLeafC (Identity .@ f)

-- | map over a nested leaf matrix with state
mapLeafS ::
  LeafC ns =>
  (FinMat ns -> c -> Vec (TP.LastT ns) a -> (c, b)) ->
  c ->
  Mat ns a ->
  (c, Mat (TP.InitT ns) b)
mapLeafS f c0 = swap . flip S.runState c0 . traverseLeafC (\i a -> S.state $ \c -> swap (f i c a))

-- | map over a nested leaf matrix only allowing changes to "a" and access to user state
mapLeafSimpleS ::
  LeafC ns =>
  (FinMat ns -> c -> Vec (TP.LastT ns) a -> (c, Vec (TP.LastT ns) b)) ->
  c ->
  Mat ns a ->
  (c, Mat ns b)
mapLeafSimpleS f c0 =
  second fromLeavesInternalC . swap . flip S.runState c0 . traverseLeafC (\i a -> S.state $ \c -> swap (f i c a))

-- | fold over a nested leaf matrix
foldLeaf ::
  LeafC ns =>
  (FinMat ns -> c -> Vec (TP.LastT ns) a -> c) ->
  c ->
  Mat ns a ->
  c
foldLeaf f = fst .@ mapLeafS g
 where
  g fn c m = (f fn c m, ())

-- | convert to nested matrix with 1d leaves
toLeaves ::
  LeafC ns =>
  Mat ns a ->
  Mat (TP.InitT ns) (Vec (TP.LastT ns) a)
toLeaves = mapLeaf (const id)

-- | methods for accessing all the leaf rows of a matrix: restricted to 2d hence this class
class LeafC ns where
  traverseLeafC ::
    Applicative m =>
    (FinMat ns -> Vec (TP.LastT ns) a -> m b) ->
    Mat ns a ->
    m (Mat (TP.InitT ns) b)
  fromLeavesInternalC ::
    Mat (TP.InitT ns) (Vec (TP.LastT ns) a) ->
    Mat ns a

instance
  GL.TypeError ( 'GL.Text "LeafC '[]: rows for empty indices are not supported") =>
  LeafC '[]
  where
  traverseLeafC = compileError "LeafC:traverseLeafC"
  fromLeavesInternalC = compileError "LeafC:fromLeavesInternalC"

instance
  GL.TypeError ( 'GL.Text "LeafC: rows for 1D are not supported") =>
  LeafC '[n]
  where
  traverseLeafC = compileError "LeafC:traverseLeafC"
  fromLeavesInternalC = compileError "LeafC:fromLeavesInternalC"

instance LeafC (n ': m ': ns) where
  traverseLeafC f w@(Mat _ (n :| ps)) =
    case ps of
      m : ns ->
        let (ny0, nx) = unsnoc1 (m :| ns)
            ny = n :| ny0
            g x = S.state $ \i -> (f (frp $ mkFinMat i (n :| m : ns)) x, i + unP nx)
            zs = frp $ chunkNVMat (units1 (productP ny)) (nx :| []) w
            tbs = sequenceA $ flip S.evalState 0 $ traverse g zs
         in (\zz -> MatIU (V.fromList (N.toList zz)) ny) <$> tbs
      [] -> programmError "traverseLeafC: missing indices"

  fromLeavesInternalC = coerce . concatMat

-- | get the start index for each row in a mat
finMatRows :: forall ns. NS ns => NonEmpty (FinMat ns)
finMatRows =
  let (xs, _) = unsnoc1 (fromNSP @ns)
      ns = appendL1 xs (_1P :| [])
      fns = sequenceA $ N.map enumTo1 ns
   in forceRightP "finMatRows" $ traverse1 nonEmptyToFinMat fns

-- | reverse each row in a matrix
reverseRows :: LeafC ns => Mat ns a -> Mat ns a
reverseRows = mapLeafSimple (\_ (MatUnsafe v ps) -> MatUnsafe (V.reverse v) ps)

-- | sort each row of a mat using underlying 'Vec'
sortByRows :: LeafC ns => (a -> a -> Ordering) -> Mat ns a -> Mat ns a
sortByRows f = frp . wrapRows1 (N.sortBy f)

-- | visit each leaf row with a function from a nonempty to a nonempty list
wrapRows1 :: LeafC ns => (NonEmpty a -> NonEmpty b) -> Mat ns a -> Either String (Mat ns b)
wrapRows1 f = traverseLeafSimple (const (wrap1 f))

-- | reverse the dimensions of a matrix
reverseDim :: Mat ns a -> Mat (ReverseT ns) a
reverseDim (Mat v ps) = MatIU v (N.reverse ps)

-- | resize a mat
redim :: forall ms ns a. (NS ms, ProductT ns ~ ProductT ms) => Mat ns a -> Mat ms a
redim (Mat v _) = MatU v (fromNSP @ms)

{- | describes the resulting type of taking a slice from the mat
 but the indices must match pointwise unlike SliceT so we can use the concrete FinMat to specify the indices
-}
type SliceT' :: [Nat] -> [Nat] -> Type -> Type
type family SliceT' ns' ns a where
  SliceT' '[] (_ ': _) _ = GL.TypeError ( 'GL.Text "SliceT' '[] (_ ': _): not defined for empty indices ns'")
  SliceT' (_ ': _) '[] _ = GL.TypeError ( 'GL.Text "SliceT' (_ ': _) '[]: not defined for empty indices ns")
  SliceT' '[] '[] _ = GL.TypeError ( 'GL.Text "SliceT' '[] '[]: not defined for empty indices ns and ns'")
  SliceT' '[n] '[n] a = a
  SliceT' (_ ': n' ': ns') '[_] _ =
    GL.TypeError
      ( 'GL.Text "SliceT': there are more ns' indices (lhs) than the actual matrix ns indices (rhs):"
          'GL.:<>: 'GL.Text " extra ns'="
          'GL.:<>: 'GL.ShowType (n' ': ns')
      )
  SliceT' '[n] (n ': n1 ': ns) a = Mat (n1 ': ns) a
  SliceT' (n ': n1' ': ns') (n ': n1 ': ns) a = SliceT' (n1' ': ns') (n1 ': ns) a
-- todo: this condition doesnt fire in SliceC'
-- sliceC' (finMatC @(NN 11) @(NN 29)) (mm @(NN 235))
  SliceT' (n' ': _) (n ': _) _ =
    GL.TypeError
      ( 'GL.Text "SliceT': indices need to match pointwise:"
          'GL.:<>: 'GL.Text "ie n' /= n:"
          'GL.:<>: 'GL.ShowType n'
          'GL.:<>: 'GL.Text " /= "
          'GL.:<>: 'GL.ShowType n
      )

{- | allows viewing and updating a slice of a mat using concrete indices
 inference is better with n ~ n' but since we have committed to a instance
 we are missing nice errors when the indices dont match: eg
 sliceC' @'[1] @'[7] (FinMat 0 (_7P :| [])) (mm @(NN 7))
-}
type SliceC' :: [Nat] -> [Nat] -> Constraint
class SliceC' ns' ns where
  sliceC' :: FinMat ns' -> Mat ns a -> SliceT' ns' ns a
  sliceUpdateC' :: FinMat ns' -> Mat ns a -> SliceT' ns' ns a -> Mat ns a

instance GL.TypeError ( 'GL.Text "SliceC' '[] (n ': ns): empty indices ns'") => SliceC' '[] (n ': ns) where
  sliceC' = compileError "sliceC'"
  sliceUpdateC' = compileError "sliceUpdateC'"
instance GL.TypeError ( 'GL.Text "SliceC' (n' ': ns') '[]: empty indices ns") => SliceC' (n' ': ns') '[] where
  sliceC' = compileError "sliceC'"
  sliceUpdateC' = compileError "sliceUpdateC'"
instance GL.TypeError ( 'GL.Text "SliceC' '[] '[]: empty indices ns and ns'") => SliceC' '[] '[] where
  sliceC' = compileError "sliceC'"
  sliceUpdateC' = compileError "sliceUpdateC'"

instance n ~ n' => SliceC' '[n'] '[n] where
  sliceC' (FinMat i _) (Mat v _) =
    case v V.!? i of
      Nothing -> programmError $ "sliceC': index " ++ show i ++ " out of bounds"
      Just a -> a
  sliceUpdateC' (FinMat i _) (Mat v ps) b =
    let (v1, v2) = V.splitAt i v
     in case V.uncons v2 of
          Just (_, v3) -> MatIU (v1 <> V.cons b v3) ps
          Nothing -> programmError $ "sliceUpdateC': index " ++ show i ++ " out of bounds"
instance n ~ n' => SliceC' '[n'] (n ': m ': ns) where
  sliceC' (FinMat i _) (Mat v (_ :| ps)) =
    case ps of
      m : ns ->
        let ps1 = m :| ns
            len1 = productPInt ps1
         in MatIU (V.slice (i * len1) len1 v) ps1
      [] -> programmError $ "sliceC': index " ++ show i ++ ": missing indices"

  sliceUpdateC' (FinMat i0 _) (Mat v w@(_ :| ps)) b =
    let len = productPInt ps
        i = i0 + 1
        v1 = V.slice 0 ((i - 1) * len) v
        v2 = V.slice (i * len) (productPInt w - i * len) v
     in MatIU (v1 <> mVec b <> v2) w

instance
  (n ~ n', SliceC' (n1' ': ns') (n1 ': ns)) =>
  SliceC' (n ': n1' ': ns') (n' ': n1 ': ns)
  where
  sliceC' fm@(FinMat _ (_ :| n1ns')) w@(Mat _ (n :| _)) =
    let x :| xs = finMatToNonEmpty fm
        i = unP x - 1
     in case (xs, n1ns') of
          (x1 : x1s, n1 : ns') ->
            let fn1 = frp $ nonEmptyToFinMat' (x1 :| x1s) (n1 :| ns')
             in sliceC' @(n1' ': ns') @(n1 ': ns) fn1 (sliceC' @'[n'] @(n ': n1 ': ns) (frp $ mkFinMat i (n :| [])) w)
          ([], _) -> programmError "sliceC': missing ns' indices"
          (_, []) -> programmError "sliceC': missing ns indices"
  sliceUpdateC' fm@(FinMat _ (_ :| n1ns')) (Mat v w@(_ :| ps0)) b =
    -- carve out the piece that is to be updated and pass that down then patch it all back together
    let x :| xs = finMatToNonEmpty fm
        i = unP x
     in case (ps0, xs, n1ns') of
          (_ : ns, x1 : x1s, n1 : ns') ->
            let fn1 = frp $ nonEmptyToFinMat' (x1 :| x1s) (n1 :| ns')
                ps1 = n1 :| ns
                len = productPInt ps1
                v1 = V.slice 0 ((i - 1) * len) v
                v2 = V.slice (i * len) (productPInt w - i * len) v
                m1 = MatIU (V.slice ((i - 1) * len) len v) ps1
                mx = sliceUpdateC' @(n1' ': ns') @(n1 ': ns) fn1 m1 b
             in MatIU (v1 <> mVec mx <> v2) w
          ([], _, _) -> programmError "sliceUpdateC': missing matrix indices"
          (_, [], _) -> programmError "sliceUpdateC': missing ns' indices"
          (_, _, []) -> programmError "sliceUpdateC': missing finmat indices"

instance (GL.TypeError ( 'GL.Text "SliceC': too many indices ns': length ns' > length ns")) => SliceC' (n' ': n1' ': ns') '[n] where
  sliceC' = compileError "sliceC'"
  sliceUpdateC' = compileError "sliceUpdateC'"

-- | describes the resulting type of taking a slice from the matrix
type SliceToFinMatT :: [Nat] -> [Nat] -> [Nat]
type family SliceToFinMatT is ns where
  SliceToFinMatT (_ ': _) '[] =
    GL.TypeError ( 'GL.Text "SliceToFinMatT (_ ': _) '[]: 'is' is empty")
  SliceToFinMatT '[] (_ ': _) =
    GL.TypeError ( 'GL.Text "SliceToFinMatT '[] (_ ': _): 'ns' is empty")
  SliceToFinMatT '[] '[] =
    GL.TypeError ( 'GL.Text "SliceToFinMatT '[] '[]: 'is' and 'ns' are empty")
  SliceToFinMatT '[_] '[n] = '[n]
  SliceToFinMatT (_ ': i ': is) '[_] =
    GL.TypeError
      ( 'GL.Text "SliceToFinMatT: 'is' is larger in length than 'ns':"
          'GL.:<>: 'GL.Text " extra 'is'="
          'GL.:<>: 'GL.ShowType (i ': is)
      )
  SliceToFinMatT '[_] (n ': _ ': _) = '[n]
  SliceToFinMatT (_ ': i1 ': is) (n ': n1 ': ns) = n ': SliceToFinMatT (i1 ': is) (n1 ': ns)

-- | converts a typelevel slice to a concrete 'FinMat'
sliceToFinMat ::
  forall is ns.
  (NS (SliceToFinMatT is ns), NS is, NS ns) =>
  FinMat (SliceToFinMatT is ns)
sliceToFinMat =
  let is = fromNSP @is
      ns = fromNSP @ns
   in frp $ nonEmptyToFinMat (N.zipWith const is ns)

-- | get a slice by converting a typelevel slice to a concrete FinMat based slice
slice ::
  forall is ns a z.
  (z ~ SliceToFinMatT is ns, NS is, NS ns, NS z, SliceC' z ns) =>
  Mat ns a ->
  SliceT' z ns a
slice = sliceC' (sliceToFinMat @is @ns)

-- | update a slice by converting a typelevel slice to a concrete FinMat based slice
sliceUpdate ::
  forall is ns a z.
  (z ~ SliceToFinMatT is ns, NS is, NS ns, NS z, SliceC' z ns) =>
  Mat ns a ->
  SliceT' z ns a ->
  Mat ns a
sliceUpdate = sliceUpdateC' (sliceToFinMat @is @ns)

-- | describes the resulting type of taking a slice from the mat
type SliceT :: [Nat] -> [Nat] -> Type -> Type
type family SliceT is ns a where
  SliceT '[] (_ ': _) _ = GL.TypeError ( 'GL.Text "SliceT '[] (_ ': _): not defined for empty indices ns'")
  SliceT (_ ': _) '[] _ = GL.TypeError ( 'GL.Text "SliceT (_ ': _) '[]: not defined for empty indices ns")
  SliceT '[] '[] _ = GL.TypeError ( 'GL.Text "SliceT '[] '[]: not defined for empty indices ns and ns'")
  SliceT '[_] '[_] a = a
  SliceT (_ ': i ': is) '[_] _ =
    GL.TypeError
      ( 'GL.Text "SliceT: 'is' must be a smaller in length than 'ns'"
          'GL.:<>: 'GL.Text " extra 'is'="
          'GL.:<>: 'GL.ShowType (i ': is)
      )
  SliceT '[_] (_ ': n1 ': ns) a = Mat (n1 ': ns) a
  SliceT (_ ': i1 ': is) (_ ': n1 ': ns) a = SliceT (i1 ': is) (n1 ': ns) a

-- | allows viewing and updating a slice of a mat
type SliceC :: [Nat] -> [Nat] -> Constraint
class SliceC is ns where
  sliceC :: Mat ns a -> SliceT is ns a
  sliceUpdateC :: Mat ns a -> SliceT is ns a -> Mat ns a

instance GL.TypeError ( 'GL.Text "SliceC '[] (n ': ns): empty indices ns'") => SliceC '[] (n ': ns) where
  sliceC = compileError "SliceC:sliceC"
  sliceUpdateC = compileError "sliceUpdateC"
instance GL.TypeError ( 'GL.Text "SliceC (n' ': ns') '[]: empty indices ns") => SliceC (n' ': ns') '[] where
  sliceC = compileError "SliceC:sliceC"
  sliceUpdateC = compileError "SliceC:sliceUpdateC"
instance GL.TypeError ( 'GL.Text "SliceC '[] '[]: empty indices ns and ns'") => SliceC '[] '[] where
  sliceC = compileError "SliceC:sliceC"
  sliceUpdateC = compileError "SliceC:sliceUpdateC"

instance FinT i n => SliceC '[i] '[n] where
  sliceC (Mat v _) =
    let i = fromN @i - 1
     in case v V.!? i of
          Nothing -> programmError $ "sliceC: index " ++ show i ++ " out of bounds"
          Just a -> a
  sliceUpdateC (Mat v ps) b =
    let i = fromN @i - 1
        (v1, v2) = V.splitAt i v
     in case V.uncons v2 of
          Just (_, v3) -> MatIU (v1 <> V.cons b v3) ps
          Nothing -> programmError $ "sliceUpdateC: index " ++ show i ++ " out of bounds"
instance FinT i n => SliceC '[i] (n ': m ': ns) where
  sliceC (Mat v (_ :| ps)) =
    case ps of
      m : ns ->
        let i = fromN @i - 1
            ps1 = m :| ns
            len1 = productPInt ps1
         in MatIU (V.slice (i * len1) len1 v) ps1
      [] -> programmError $ "sliceUpdateC: index " ++ show (fromN @i) ++ ": missing indices"

  sliceUpdateC (Mat v w@(_ :| ps)) b =
    let i = fromN @i
        len = productPInt ps
        v1 = V.slice 0 ((i - 1) * len) v
        v2 = V.slice (i * len) (productPInt w - i * len) v
     in MatIU (v1 <> mVec b <> v2) w

instance
  (FinT i n, SliceC (i1 ': is) (n1 ': ns)) =>
  SliceC (i ': i1 ': is) (n ': n1 ': ns)
  where
  sliceC w =
    sliceC @(i1 ': is) @(n1 ': ns) (sliceC @'[i] @(n ': n1 ': ns) w)
  sliceUpdateC (Mat v w@(_ :| ps0)) b =
    -- carve out the piece that is to be updated and pass that down then patch it all back together
    case ps0 of
      n1 : ns ->
        let i = fromN @i
            ps1 = n1 :| ns
            len = productPInt ps1
            v1 = V.slice 0 ((i - 1) * len) v
            v2 = V.slice (i * len) (productPInt w - i * len) v
            m1 = MatIU (V.slice ((i - 1) * len) len v) ps1
            mx = sliceUpdateC @(i1 ': is) @(n1 ': ns) m1 b
         in MatIU (v1 <> mVec mx <> v2) w
      [] -> programmError $ "sliceUpdateC: index " ++ show (fromN @i) ++ ": missing indices"

instance (GL.TypeError ( 'GL.Text "too many indices 'is': length is > length ns")) => SliceC (i ': i1 ': is) '[n] where
  sliceC = compileError "sliceC (2)"
  sliceUpdateC = compileError "sliceUpdateC (2)"

-- | a lens indexing the outermost slice
_row ::
  forall (i :: Nat) (ns :: [Nat]) a.
  (SliceC '[i] ns) =>
  Lens' (Mat ns a) (SliceT '[i] ns a)
_row = ixSlice @'[i]

-- | a lens for acccessing a column
_col ::
  forall (i :: Nat) n m ns a.
  (FinT i m) =>
  Lens' (Mat (n ': m ': ns) a) (Mat (n ': ns) a)
_col = _transposeMat . _row @i

-- | a lens for accessing a slice of a mat
ixSlice ::
  forall (is :: [Nat]) (ns :: [Nat]) a.
  (SliceC is ns) =>
  Lens' (Mat ns a) (SliceT is ns a)
ixSlice =
  lens
    (sliceC @is)
    (sliceUpdateC @is)

-- | a lens indexing a row using a concrete index 'Fin'
_row' ::
  forall (n :: Nat) (ns :: [Nat]) a.
  (SliceC' '[n] ns) =>
  Fin n ->
  Lens' (Mat ns a) (SliceT' '[n] ns a)
_row' (Fin i _) = ixSlice' @'[n] (frp $ mkFinMat (unP i - 1) (succP i :| []))

-- | a lens for acccessing a column
_col' ::
  forall n m ns a.
  Fin m ->
  Lens' (Mat (n ': m ': ns) a) (Mat (n ': ns) a)
_col' fn = _transposeMat . _row' fn

-- | a lens for accessing a slice of a mat
ixSlice' ::
  forall (ns' :: [Nat]) (ns :: [Nat]) a.
  (SliceC' ns' ns) =>
  FinMat ns' ->
  Lens' (Mat ns a) (SliceT' ns' ns a)
ixSlice' fm =
  lens
    (sliceC' @ns' fm)
    (sliceUpdateC' @ns' fm)

-- | break up into rows
rows ::
  forall n m ns a.
  Mat (n ': m ': ns) a ->
  Vec n (Mat (m ': ns) a)
rows w@(Mat _ (n :| ps)) =
  case ps of
    m : ns ->
      let zs = frp $ chunkNVMat (unitsF @[] n) (m :| ns) w
       in MatIU (V.fromList zs) (n :| [])
    [] -> programmError "rows: missing indices"

-- | unbust from rows @see 'rows'
unrows ::
  forall n m ns a.
  Vec n (Mat (m ': ns) a) ->
  Mat (n ': m ': ns) a
unrows = concatMat

-- | split up a matrix into matrix chunks of dimension "ps" and fill a container "tz"
chunkNVMat ::
  forall ns t x a z.
  Traversable t =>
  t z ->
  NonEmpty Pos ->
  Mat x a ->
  Either String (t (Mat ns a))
chunkNVMat tz ps = (fmap . fmap) (`MatIU` ps) . chunkNV tz (productP ps) . mVec

-- | split up a vector into chunks of size "n" and fill a container "tz"
chunkNV ::
  forall t a z.
  Traversable t =>
  t z ->
  Pos ->
  Vector a ->
  Either String (t (Vector a))
chunkNV tz (Pos n) = chunkN' g tz
 where
  g = Right . swap . V.splitAt n

-- 4 conditions:
--   1: (n:|[m]) a X (m:|[p]) b == (n:|[p]) (a->b->c)
--   2: (n:|m:q:ns) a X (m:|[p]) b == (n:|[p]) ((q:|ns) a -> b -> c)
--   3: (n:|m:q:ns) a X (m:|p:r:xs) b == (n:|[p]) ((q:|ns) a -> (r:|xs) b -> c)
--   4: (n:|[m]) a X (m:|p:r:xs) b == (n:|[p]) (a -> (r:|xs) b -> c)

-- | generalised dot product
type DotC :: [Nat] -> [Nat] -> Type -> Type -> Type -> Type -> Constraint
class
  DotC ns ns' a b fa fb
    | ns ns' a -> fa
    , ns ns' b -> fb
    , ns ns' fa -> a
    , ns ns' fb -> b
    , fa fb a b -> ns ns'
  where
  dotC ::
    (fa -> fb -> c) ->
    (NonEmpty c -> d) ->
    Mat (n ': m ': ns) a ->
    Mat (m ': p ': ns') b ->
    Mat2 n p d

instance DotC '[] '[] a b a b where
  dotC = dot
instance DotC (q ': ns) '[] a b (Mat (q ': ns) a) b where
  dotC f g m1 m2 = dot f g (toMat2 m1) m2
instance DotC '[] (r ': xs) a b a (Mat (r ': xs) b) where
  dotC f g m1 m2 = dot f g m1 (toMat2 m2)
instance DotC (q ': ns) (r ': xs) a b (Mat (q ': ns) a) (Mat (r ': xs) b) where
  dotC f g m1 m2 = dot f g (toMat2 m1) (toMat2 m2)

-- | base case for generalised dot product
dot ::
  forall n m p a b c d.
  (a -> b -> c) ->
  (NonEmpty c -> d) ->
  Mat2 n m a ->
  Mat2 m p b ->
  Mat2 n p d
dot f g w1@(Mat _ (n :| ps1)) w2@(Mat _ (_ :| ps2)) =
  case (ps1, ps2) of
    ([m], [p]) ->
      let z1 = frp $ chunkNLen1 n m w1
          z2 = N.transpose $ frp $ chunkNLen1 m p w2
          w = liftA2 ((g . frp) .@ zipWithExact f) z1 z2
       in MatIU (V.fromList $ N.toList w) (n :| [p])
    o -> programmError $ "dot: missing indices " ++ show o

-- | multiply two matrices together
multMat ::
  forall n m p a.
  Num a =>
  Mat2 n m a ->
  Mat2 m p a ->
  Mat2 n p a
multMat = dot (*) sum1

-- | delete a row
deleteRow ::
  forall (i :: Nat) (n :: Nat) (ns :: [Nat]) a.
  FinT i (1 GN.+ n) =>
  Mat (1 GN.+ n ': ns) a ->
  Mat (n ': ns) a
deleteRow = deleteRow' (finC @i @(1 GN.+ n))

-- | delete a row using a concrete index
deleteRow' ::
  forall n ns a.
  Fin (1 GN.+ n) ->
  Mat (1 GN.+ n ': ns) a ->
  Mat (n ': ns) a
deleteRow' (Fin (Pos i) _) (Mat v (sn :| ps)) =
  let n = frp $ predP sn
      n1 = productPInt ps
      s = (i - 1) * n1
      v1 = V.slice 0 s v
      v2 = V.slice (s + n1) (productPInt (sn :| ps) - s - n1) v
   in MatIU (v1 <> v2) (n :| ps)

-- | delete a row from a matrix
insertRow ::
  forall i n m ns a.
  FinT i (1 GN.+ n) =>
  Mat (m ': ns) a ->
  Mat (n ': m ': ns) a ->
  Mat (1 GN.+ n ': m ': ns) a
insertRow = insertRow' (finC @i @(1 GN.+ n))

-- | same as 'insertRow' but using a typelevel witness for the index
insertRow' ::
  forall n m ns a.
  Fin (1 GN.+ n) ->
  Mat (m ': ns) a ->
  Mat (n ': m ': ns) a ->
  Mat (1 GN.+ n ': m ': ns) a
insertRow' (Fin (Pos i) _) (Mat v0 _) (Mat v (p :| ps)) =
  let s = (i - 1) * productPInt ps
      v1 = V.slice 0 s v
      v2 = V.slice s (productPInt (p :| ps) - s) v
   in MatIU (v1 <> v0 <> v2) (succP p :| ps)

-- | delete a column from a matrix (2d or higher)
deleteCol ::
  forall (i :: Nat) (n :: Nat) (n1 :: Nat) ns a.
  FinT i (1 GN.+ n1) =>
  Mat (n ': (1 GN.+ n1) ': ns) a ->
  Mat (n ': n1 ': ns) a
deleteCol = deleteCol' (finC @i @(1 GN.+ n1))

-- | same as 'deleteCol' but using a typelevel witness for the index
deleteCol' ::
  forall (n :: Nat) (n1 :: Nat) ns a.
  Fin (1 GN.+ n1) ->
  Mat (n ': (1 GN.+ n1) ': ns) a ->
  Mat (n ': n1 ': ns) a
deleteCol' fn = transposeMat @n1 @n . deleteRow' @n1 fn . transposeMat @n @(1 GN.+ n1)

-- | insert a column into a mat (2d and above)
insertCol ::
  forall (i :: Nat) (n :: Nat) (n1 :: Nat) ns a.
  FinT i (1 GN.+ n1) =>
  Mat (n ': ns) a ->
  Mat (n ': n1 ': ns) a ->
  Mat (n ': (1 GN.+ n1) ': ns) a
insertCol = insertCol' (finC @i @(1 GN.+ n1))

-- | same as 'insertCol' but using a typelevel witness 'Fin'
insertCol' ::
  forall (n :: Nat) (n1 :: Nat) ns a.
  Fin (1 GN.+ n1) ->
  Mat (n ': ns) a ->
  Mat (n ': n1 ': ns) a ->
  Mat (n ': (1 GN.+ n1) ': ns) a
insertCol' fn v = transposeMat @(1 GN.+ n1) @n . insertRow' fn v . transposeMat @n @n1

-- | swaps mat rows (1d or more)
swapRow ::
  forall (i :: Nat) (j :: Nat) (n :: Nat) ns a.
  (FinT i n, FinT j n) =>
  Mat (n ': ns) a ->
  Mat (n ': ns) a
swapRow = swapRow' (finC @i) (finC @j)

-- | swaps mat rows (1d or more)
swapRow' ::
  forall (n :: Nat) ns a.
  Fin n ->
  Fin n ->
  Mat (n ': ns) a ->
  Mat (n ': ns) a
swapRow' (Fin ix _) (Fin jx _) z@(Mat v w@(_ :| ps)) =
  let (Pos i, Pos j) = bool id swap (ix > jx) (ix, jx)
      len = productPInt ps
   in if i == j
        then z
        else
          let s0 = (i - 1) * len
              s1 = (j - 1) * len
              x1 = V.slice 0 s0 v
              x2 = V.slice s0 len v
              x3 = V.slice (s0 + len) (s1 - s0 - len) v
              x4 = V.slice s1 len v
              x5 = V.slice (s1 + len) (productPInt w - s1 - len) v
           in MatIU (x1 <> x4 <> x3 <> x2 <> x5) w

-- | swaps mat rows (2d or more)
swapCol ::
  forall (i :: Nat) (j :: Nat) (n :: Nat) (n1 :: Nat) ns a.
  (FinT i n1, FinT j n1) =>
  Mat (n ': n1 ': ns) a ->
  Mat (n ': n1 ': ns) a
swapCol = swapCol' (finC @i) (finC @j)

-- | swaps mat rows (2d or more)
swapCol' ::
  forall (n :: Nat) (n1 :: Nat) ns a.
  Fin n1 ->
  Fin n1 ->
  Mat (n ': n1 ': ns) a ->
  Mat (n ': n1 ': ns) a
swapCol' fni fnj = transposeMat . swapRow' fni fnj . transposeMat

-- | swaps a single value "a" from any location to any other location using type level indexes
swapMat ::
  forall (is :: [Nat]) (js :: [Nat]) ns a.
  (FinMatC is ns, FinMatC js ns) =>
  Mat ns a ->
  Mat ns a
swapMat = swapMat' (finMatC @is @ns) (finMatC @js @ns)

-- | same as 'swapMat' but using typelevel witnesses 'FinMat'
swapMat' ::
  forall ns a.
  FinMat ns ->
  FinMat ns ->
  Mat ns a ->
  Mat ns a
swapMat' (FinMat i _) (FinMat j _) (Mat v ps) =
  MatIU (V.update v (V.fromList [(i, v V.! j), (j, v V.! i)])) ps

-- | append two matrices vertically
appendV ::
  Mat (n ': ns) a ->
  Mat (n' ': ns) a ->
  Mat ((n GN.+ n') ': ns) a
appendV (Mat v (p :| ps)) (Mat v1 (p1 :| _)) =
  MatIU (v <> v1) ((p +! p1) :| ps)

-- | append two matrices horizontally
appendH ::
  forall n m m' ns a.
  Mat (n ': m ': ns) a ->
  Mat (n ': m' ': ns) a ->
  Mat (n ': (m GN.+ m') ': ns) a
appendH w@(Mat _ (n :| ps)) w1@(Mat _ (n' :| ps1))
  | n == n' =
      case (ps, ps1) of
        ([], _) -> programmError "appendH:lhs missing indices"
        (_, []) -> programmError "appendH:rhs missing indices"
        (m : ns, m' : ns')
          | ns == ns' ->
              let x1 = frp $ chunkNV (unitsF n) (productP (m :| ns)) (mVec w)
                  x2 = frp $ chunkNV (unitsF @[] n) (productP (m' :| ns')) (mVec w1)
                  ret = frp $ zipWithExact (<>) x1 x2
                  ps2 = n :| ([m +! m'] <> ns)
               in MatIU (V.concat ret) ps2
          | otherwise -> programmError $ "appendH:ns/=ns' " ++ show (ns, ns')
  | otherwise = programmError $ "appendH: n/=n' " ++ show (n, n')

-- | return a mat as a permutation of a list (1d only) todo: extend to multidimensions
permutationsMat :: forall n a. Vec n a -> Mat2 (FacT n) n a
permutationsMat (Mat v (p :| _)) =
  let ret = L.permutations (V.toList v)
      lhs = productP (enumTo1 p)
   in MatIU (V.fromList $ concat ret) (lhs :| [p])

-- | find all elements in a mat that match the predicate
findMatElems :: NS ns => (a -> Bool) -> Mat ns a -> [(FinMat ns, a)]
findMatElems p = ifoldMap (\i a -> bool [] [(i, a)] (p a))

-- | generate a 'Mat' with the given past and future rep values and a user state
buildMat ::
  forall ns a b.
  NS ns =>
  ([FinMat ns] -> [FinMat ns] -> b -> FinMat ns -> (b, a)) ->
  b ->
  (b, Mat ns a)
buildMat = buildRepL

-- | cartesian product of two matrices with a combining function
cartesian ::
  (a -> b -> c) ->
  Mat (n ': ns) a ->
  Mat (n' ': ns') b ->
  Mat (n ': ns TP.++ n' ': ns') c
cartesian f (Mat v ps) (Mat v1 ps1) =
  MatIU (liftA2 f v v1) (ps <> ps1)

-- | lens for bulk updates/gets on a matrix
bulkMat :: Vec x (FinMat ns) -> Lens' (Mat ns a) (Vec x a)
bulkMat fins =
  lens
    (getsMat fins)
    (\m lst -> setsMat (zipMat fins lst) m)

-- | bulk updates on a mat
updatesMat ::
  forall ns t a b.
  Foldable t =>
  (FinMat ns -> a -> b -> a) ->
  t (FinMat ns, b) ->
  Mat ns a ->
  Mat ns a
updatesMat f = flip (L.foldl' g)
 where
  g m (fm, b) = updateMat (\a -> f fm a b) fm m

-- | bulk gets from a mat: replaces the container of indices with the corresponding values
getsMat :: forall ns a t. Functor t => t (FinMat ns) -> Mat ns a -> t a
getsMat lst m = (`indexMat` m) <$> lst

-- | bulk updates on a mat
setsMat ::
  forall ns t a.
  Foldable t =>
  t (FinMat ns, a) ->
  Mat ns a ->
  Mat ns a
setsMat = flip (L.foldl' g)
 where
  g :: Mat ns a -> (FinMat ns, a) -> Mat ns a
  g m (fm, a) = setMat a fm m

-- | convert a matrix to a nested tuple
type MatTupleT :: [Nat] -> Type -> Type
type family MatTupleT ns a where
  MatTupleT '[] _ = GL.TypeError ('GL.Text "MatTupleT '[]: undefined for empty indices")
  MatTupleT '[n] a = ListTupleT n a
  MatTupleT (n ': n1 ': ns) a = ListTupleT n (MatTupleT (n1 ': ns) a)

-- | convert a between a matrix and a nested tuple
type MatTupleC :: [Nat] -> Type -> Constraint
class MatTupleC ns a where
  toTupleC ::
    Mat ns a ->
    -- | convert a 'Mat' to a nested tuple
    MatTupleT ns a
  fromTupleC ::
    MatTupleT ns a ->
    -- | convert a well-formed nested tuple of type "a" to 'Mat'
    Mat ns a
  fmapTupleMatC ::
    (a -> b) ->
    MatTupleT ns a ->
    -- | fmap over a well-formed nested tuple
    MatTupleT ns b
  traversalTupleMatC ::
    -- | traversal over a well-formed nested tuple
    Traversal (MatTupleT ns a) (MatTupleT ns b) a b

instance GL.TypeError ('GL.Text "MatTupleC '[]: undefined for empty indices") => MatTupleC '[] a where
  toTupleC = compileError "MatTupleC:toTupleC"
  fromTupleC = compileError "MatTupleC:fromTupleC"
  fmapTupleMatC = compileError "MatTupleC:fmapTupleMatC"
  traversalTupleMatC = compileError "MatTupleC:traversalTupleMatC"

instance ListTupleCInternal n => MatTupleC '[n] a where
  toTupleC  = toTupleCInternal
  fromTupleC = fromTupleCInternal
  fmapTupleMatC = fmapTupleInternal
  traversalTupleMatC = traversalTupleCInternal
instance
  (ListTupleCInternal n, NS (n1 ': ns), MatTupleC (n1 ': ns) a) =>
  MatTupleC (n ': n1 ': ns) a
  where
  toTupleC lst = toTupleCInternal @n (fmap (toTupleC @(n1 ': ns)) (rows @n lst))
  fromTupleC x =
    let Mat v (n' :| _) = fromTupleCInternal (fmapTupleInternal (fromTupleC @(n1 ': ns)) x)
        xs = foldMap mVec v
        ps1 = n' N.<| fromNSP @(n1 ': ns)
     in MatIU @(n ': n1 ': ns) xs ps1

  fmapTupleMatC f x = fmapTupleInternal (fmapTupleMatC @(n1 ': ns) f) x
  traversalTupleMatC afa = traversalTupleCInternal @n (traversalTupleMatC @(n1 ': ns) afa)

-- | fmap over a n-tuple
fmapTupleInternal :: ListTupleCInternal n => (a -> b) -> ListTupleT n a -> ListTupleT n b
fmapTupleInternal f = runIdentity . traversalTupleCInternal (Identity . f)

-- | conversions between n-tuple and 'Vec'
type ListTupleCInternal :: Nat -> Constraint
class ListTupleCInternal n where
  -- | convert a 'Vec' to a tuple
  toTupleCInternal :: Vec n a -> ListTupleT n a

  -- | convert a tuple of type "a" to 'Vec'
  fromTupleCInternal :: ListTupleT n a -> Vec n a

  -- | traversal over a tuple
  traversalTupleCInternal :: Traversal (ListTupleT n a) (ListTupleT n b) a b

instance ListTupleCInternal 1 where
  toTupleCInternal (toNonEmptyMat -> a :| []) = One a
  toTupleCInternal _o = programmError "ListTupleCInternal 1"
  fromTupleCInternal (One a) = se1 a
  traversalTupleCInternal afa (One a) = One <$> afa a
instance ListTupleCInternal 2 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2]) = (a1, a2)
  toTupleCInternal _o = programmError "ListTupleCInternal 2"
  fromTupleCInternal (a1, a2) = a1 .| a2
  traversalTupleCInternal afa (a1, a2) = (,) <$> afa a1 <*> afa a2
instance ListTupleCInternal 3 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3]) = (a1, a2, a3)
  toTupleCInternal _o = programmError "ListTupleCInternal 3"
  fromTupleCInternal (a1, a2, a3) = a1 .: a2 .| a3
  traversalTupleCInternal afa (a1, a2, a3) = (,,) <$> afa a1 <*> afa a2 <*> afa a3
instance ListTupleCInternal 4 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4]) = (a1, a2, a3, a4)
  toTupleCInternal _o = programmError "ListTupleCInternal 4"
  fromTupleCInternal (a1, a2, a3, a4) = a1 .: a2 .: a3 .| a4
  traversalTupleCInternal afa (a1, a2, a3, a4) = (,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4
instance ListTupleCInternal 5 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4, a5]) = (a1, a2, a3, a4, a5)
  toTupleCInternal _o = programmError "ListTupleCInternal 5"
  fromTupleCInternal (a1, a2, a3, a4, a5) = a1 .: a2 .: a3 .: a4 .| a5
  traversalTupleCInternal afa (a1, a2, a3, a4, a5) = (,,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4 <*> afa a5
instance ListTupleCInternal 6 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4, a5, a6]) = (a1, a2, a3, a4, a5, a6)
  toTupleCInternal _o = programmError "ListTupleCInternal 6"
  fromTupleCInternal (a1, a2, a3, a4, a5, a6) = a1 .: a2 .: a3 .: a4 .: a5 .| a6
  traversalTupleCInternal afa (a1, a2, a3, a4, a5, a6) = (,,,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4 <*> afa a5 <*> afa a6
instance ListTupleCInternal 7 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4, a5, a6, a7]) = (a1, a2, a3, a4, a5, a6, a7)
  toTupleCInternal _o = programmError "ListTupleCInternal 7"
  fromTupleCInternal (a1, a2, a3, a4, a5, a6, a7) = a1 .: a2 .: a3 .: a4 .: a5 .: a6 .| a7
  traversalTupleCInternal afa (a1, a2, a3, a4, a5, a6, a7) = (,,,,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4 <*> afa a5 <*> afa a6 <*> afa a7
instance ListTupleCInternal 8 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4, a5, a6, a7, a8]) = (a1, a2, a3, a4, a5, a6, a7, a8)
  toTupleCInternal _o = programmError "ListTupleCInternal 8"
  fromTupleCInternal (a1, a2, a3, a4, a5, a6, a7, a8) = a1 .: a2 .: a3 .: a4 .: a5 .: a6 .: a7 .| a8
  traversalTupleCInternal afa (a1, a2, a3, a4, a5, a6, a7, a8) = (,,,,,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4 <*> afa a5 <*> afa a6 <*> afa a7 <*> afa a8
instance ListTupleCInternal 9 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4, a5, a6, a7, a8, a9]) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  toTupleCInternal _o = programmError "ListTupleCInternal 9"
  fromTupleCInternal (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1 .: a2 .: a3 .: a4 .: a5 .: a6 .: a7 .: a8 .| a9
  traversalTupleCInternal afa (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (,,,,,,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4 <*> afa a5 <*> afa a6 <*> afa a7 <*> afa a8 <*> afa a9
instance ListTupleCInternal 10 where
  toTupleCInternal (toNonEmptyMat -> a1 :| [a2, a3, a4, a5, a6, a7, a8, a9, a10]) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  toTupleCInternal _o = programmError "ListTupleCInternal 10"
  fromTupleCInternal (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a1 .: a2 .: a3 .: a4 .: a5 .: a6 .: a7 .: a8 .: a9 .| a10
  traversalTupleCInternal afa (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (,,,,,,,,,) <$> afa a1 <*> afa a2 <*> afa a3 <*> afa a4 <*> afa a5 <*> afa a6 <*> afa a7 <*> afa a8 <*> afa a9 <*> afa a10

-- | an iso for transposing a matrix
_transposeMat :: Iso (Mat (n ': m ': ns) a) (Mat (n ': m ': ns) b) (Mat (m ': n ': ns) a) (Mat (m ': n ': ns) b)
_transposeMat = iso transposeMat transposeMat

-- | transpose a 2d or larger matrix
transposeMat :: forall n m ns a. Mat (n ': m ': ns) a -> Mat (m ': n ': ns) a
transposeMat w@(Mat _ (n :| ps)) =
  case ps of
    [] -> programmError "transposeMat"
    m : ns ->
      let ys = frp $ chunkNLen1 n (productP (m :| ns)) w
          zs = N.transpose $ N.map (chunksOf1 (productP ns)) ys
       in MatIU (V.fromList $ N.toList $ sconcat $ sconcat zs) (m :| (n : ns))

-- | validate and convert from a nested list to a matrix
nestedListToMatValidated :: forall ns x a. (x ~ ListNST ns a, ValidateNestedListC x (ValidateNestedListT x), MatConvertersC ns) => ListNST ns a -> Either String (Mat ns a)
nestedListToMatValidated w = do
  _ <- validateNestedList w
  nestedListToMatC w

-- | validate and convert from a nested nonempty list to a matrix
nestedNonEmptyToMatValidated :: forall ns x a. (x ~ NonEmptyNST ns a, ValidateNestedNonEmptyC x (ValidateNestedNonEmptyT x), MatConvertersC ns) => NonEmptyNST ns a -> Either String (Mat ns a)
nestedNonEmptyToMatValidated w = do
  _ <- validateNestedNonEmpty w
  nestedNonEmptyToMatC w

-- | class with methods to convert to and from Mat using nested structures
class MatConvertersC ns where
  -- | convert a 'Mat' to nested 'Vec'
  matToNestedVecC :: Mat ns a -> MatToNestedVecT ns a

  -- | convert a nested 'Vec' to a 'Mat'
  nestedVecToMatC :: MatToNestedVecT ns a -> Mat ns a

  -- | convert a 'Mat' to a nested list
  matToNestedListC :: Mat ns a -> ListNST ns a

  -- | convert a nested list to a 'Mat'
  nestedListToMatC :: ListNST ns a -> Either String (Mat ns a)

  -- | convert a 'Mat' to a nested nonempty list
  matToNestedNonEmptyC :: Mat ns a -> NonEmptyNST ns a

  -- | convert a nested nonempty list to a 'Mat'
  nestedNonEmptyToMatC :: NonEmptyNST ns a -> Either String (Mat ns a)

instance GL.TypeError ('GL.Text "MatConvertersC '[]: undefined for empty indices") => MatConvertersC '[] where
  matToNestedVecC = compileError "MatConvertersC"
  nestedVecToMatC = compileError "MatConvertersC"
  matToNestedListC = compileError "MatConvertersC"
  matToNestedNonEmptyC = compileError "MatConvertersC"
  nestedListToMatC = compileError "MatConvertersC"
  nestedNonEmptyToMatC = compileError "MatConvertersC"

instance PosT n => MatConvertersC '[n] where
  matToNestedVecC = id
  nestedVecToMatC = id
  matToNestedListC = toListMat
  matToNestedNonEmptyC = toNonEmptyMat
  nestedListToMatC = matImpl True
  nestedNonEmptyToMatC = matImpl True . N.toList
instance (PosT n, MatConvertersC (m ': ns)) => MatConvertersC (n ': m ': ns) where
  matToNestedVecC lst = fmap matToNestedVecC (rows @n lst)
  nestedVecToMatC lst@(Mat _ (n :| _)) =
    let zs@(Mat _ (m :| ns) :| _) = toNonEmptyMat $ fmap (nestedVecToMatC @(m ': ns)) lst
        ys = foldMap mVec zs
     in MatIU ys (n :| m : ns)
  matToNestedListC w = toListMat $ fmap (matToNestedListC @(m ': ns)) (rows @n w)
  matToNestedNonEmptyC w = toNonEmptyMat $ fmap (matToNestedNonEmptyC @(m ': ns)) (rows @n w)
  nestedListToMatC = \case
    [] -> Left "nestedListToMatC: no data"
    w : ws -> nonEmptyMatsToMat =<< traverse (nestedListToMatC @(m ': ns)) (w :| ws)
  nestedNonEmptyToMatC w = nonEmptyMatsToMat =<< traverse (nestedNonEmptyToMatC @(m ': ns)) w

-- | create a matrix of one dimension higher from rows of a sub matrix
nonEmptyMatsToMat :: forall n m ns a t. (Foldable1 t, PosT n) => t (Mat (m ': ns) a) -> Either String (Mat (n ': m ': ns) a)
nonEmptyMatsToMat (toNonEmpty -> xs@(Mat _ ps :| _)) = do
  let n = fromNP @n
  ret <- lengthExact1 n xs
  pure $ MatIU (sconcat (fmap mVec ret)) (n N.<| ps)

-- | converts mat dimensions to a nested list
type MatToNestedVecT :: [Nat] -> Type -> Type
type family MatToNestedVecT ns a where
  MatToNestedVecT '[] _ = GL.TypeError ('GL.Text "MatToNestedVecT '[]: undefined for empty indices")
  MatToNestedVecT '[n] a = Vec n a
  MatToNestedVecT (n ': n1 ': ns) a = Vec n (MatToNestedVecT (n1 ': ns) a)

-- | type synonym for the result of nesting a matrix: @see 'toND'
type MatToNDT :: Nat -> [Nat] -> Type -> Type
type MatToNDT i ns a = Mat (MatToMatNTA (NatToPeanoT i) ns) (Mat (MatToMatNTB (NatToPeanoT i) ns) a)

-- | create a nested matrix going "i" levels down: noop is not supported ie 4D matrix to a 4D matrix
matToNDImpl ::
  forall (i :: Nat) (ns :: [Nat]) a.
  PosT i =>
  Mat ns a ->
  MatToNDT i ns a
matToNDImpl w@(Mat _ ps) =
  let i = fromNP @i
      (ps1, bs) = splitAt1 i ps
   in case bs of
        y : ys ->
          let ps2 = y :| ys
              xs = frp $ chunkNVMat (unitsF (productP ps1)) ps2 w
           in MatIU (V.fromList xs) ps1
        [] -> programmError "toND:missing indices to the right"

type MatToMatNTA :: Peano -> [Nat] -> [Nat]
type family MatToMatNTA i ns where
  MatToMatNTA _ '[] =
    GL.TypeError ( 'GL.Text "MatToMatNTA '[]: empty indices")
  MatToMatNTA ( 'S 'Z) '[_] =
    GL.TypeError ( 'GL.Text "MatToMatNTA: noop as the depth 'i' is the same as the number of indices")
  MatToMatNTA ( 'S 'Z) (n ': _ ': _) = '[n]
  MatToMatNTA ( 'S _) '[_] =
    GL.TypeError ( 'GL.Text "MatToMatNTA: depth is more than the number of indices")
  MatToMatNTA ( 'S ( 'S i)) (n ': m ': ns) = n : MatToMatNTA ( 'S i) (m ': ns)

type MatToMatNTB :: Peano -> [Nat] -> [Nat]
type family MatToMatNTB i ns where
  MatToMatNTB _ '[] =
    GL.TypeError ( 'GL.Text "MatToMatNTB: empty indices")
  MatToMatNTB ( 'S 'Z) '[_] =
    GL.TypeError ( 'GL.Text "MatToMatNTB: noop as the depth 'i' is the same as the number of indices")
  MatToMatNTB ( 'S 'Z) (_ ': m ': ns) = m ': ns
  MatToMatNTB ( 'S _) '[_] =
    GL.TypeError ( 'GL.Text "MatToMatNTB: depth is more than the number of indices")
  MatToMatNTB ( 'S ( 'S i)) (_ ': m ': ns) = MatToMatNTB ( 'S i) (m ': ns)

-- | create a nd matrix using a Nat @see 'toND
toND :: forall i ns a. i <=! i => Mat ns a -> MatToNDT i ns a
toND = matToNDImpl @i

-- | create a nested 1d matrix @see 'toND
toVec :: Mat ns a -> MatToNDT 1 ns a
toVec = toND @1

-- | create a nested 2d matrix @see 'toND
toMat2 :: Mat ns a -> MatToNDT 2 ns a
toMat2 = toND @2

-- | create a nested 3d matrix @see 'toND
toMat3 :: Mat ns a -> MatToNDT 3 ns a
toMat3 = toND @3

-- | squash a single nested matrix together into one
concatMat ::
  forall (n :: Nat) (ns :: [Nat]) (m :: Nat) (ms :: [Nat]) a.
  Mat (n ': ns) (Mat (m ': ms) a) ->
  Mat (n ': (ns TP.++ m ': ms)) a
concatMat w =
  let hd :| tl = toNonEmptyMat w
   in MatIU (V.concat (map mVec (hd : tl))) (mIndices w <> mIndices hd)

-- | gets the diagonal elements of a 2d or greater square matrix: the diagonal of a n * n * ns matrix results in a n * ns matrix
diagonal :: Mat (n ': n ': ns) a -> Mat (n ': ns) a
diagonal (Mat v (n :| ps)) =
  case ps of
    _n : ns ->
      let len = productPInt ns
          xs = map (\i -> V.slice (i * (unP n + 1) * len) len v) [0 .. unP n - 1]
       in MatIU (V.concat xs) (n :| ns)
    [] -> programmError "diagonal: missing indices"

-- | take a subset of a matrix using the start and end rows
subsetRows ::
  forall i j n ns a.
  DiffTC i j n =>
  Mat (n ': ns) a ->
  Mat (DiffT i j n ': ns) a
subsetRows (Mat v (_ :| ns)) =
  let i = fromNP @i
      j = fromNP @j
      n1 = (unP i - 1) * productPInt ns
      n' = frp $ withOp2 ((-) . (+ 1)) j i
      ps1 = n' :| ns
   in MatIU (V.slice n1 (productPInt ps1) v) ps1

-- todo use FinMat versions of subsetRows and subsetCols ie not just typelevel: need typelevel for the count of rows/cols so no point

-- | take a subset of a matrix using the start and end columns
subsetCols ::
  forall i j m n ns a.
  DiffTC i j n =>
  Mat (m ': n ': ns) a ->
  Mat (m ': (DiffT i j n ': ns)) a
subsetCols = transposeMat . subsetRows @i @j . transposeMat

-- | isomorphism for nesting/unnesting a matrix one level deep
_rows ::
  forall n m ns a b.
  Iso
    (Mat (n ': m ': ns) a)
    (Mat (n ': m ': ns) b)
    (Vec n (Mat (m ': ns) a))
    (Vec n (Mat (m ': ns) b))
_rows = iso rows unrows

toListMat :: Mat ns a -> [a]
toListMat = toList

toNonEmptyMat :: Mat ns a -> NonEmpty a
toNonEmptyMat = toNonEmpty

-- | specialised version of 'readMat' for 'Vec'
readVec ::
  ( MatConvertersC '[n]
  , PosT n
  , Read [a]
  ) =>
  ReadS (Vec n a)
readVec = P.readP_to_S (readMatP defShowOpts)

-- | specialised version of 'readMat' for 'Mat2'
readMat2 ::
  ( MatConvertersC '[n, m]
  , PosT n
  , PosT m
  , Read [[a]]
  ) =>
  ReadS (Mat2 n m a)
readMat2 = P.readP_to_S (readMatP defShowOpts)

-- | read in a matrix as a nested list using default 'ShowOpts'
readMat ::
  forall ns a.
  ( MatConvertersC ns
  , NS ns
  , Read (ListNST ns a)
  ) =>
  ReadS (Mat ns a)
readMat = P.readP_to_S (readMatP defShowOpts)

instance (MatConvertersC ns, NS ns, Read (ListNST ns a)) => Read (Mat ns a) where
  readPrec = PC.readP_to_Prec (const (readMatP defShowOpts))

-- | reader for 'showFin'
readMatP ::
  forall ns a.
  ( MatConvertersC ns
  , NS ns
  , Read (ListNST ns a)
  ) =>
  ShowOpts ->
  P.ReadP (Mat ns a)
readMatP opts = do
  P.skipSpaces
  let ns = fromNSP @ns
  ns' <-
    (P.string "Mat@" *> pPositives '[' ']')
      P.+++ (P.string "Vec@" *> fmap pure pPosInt)
      P.+++ ((\n m -> n :| [m]) <$ P.string "Mat2@(" <*> pPosInt <* P.char ',' <*> pPosInt <* P.char ')')
  when (ns /= ns') P.pfail
  xs <- PC.readPrec_to_P (GR.readPrec @(ListNST ns a)) 1
  ret <- either (const P.pfail) pure (nestedListToMatC xs)
  when (addNewline ns opts) $ void $ P.char '\n'
  return ret

-- | print a matrix
prtMat :: forall ns a. (ShowMatC ns, Show a) => ShowOpts -> Mat ns a -> IO ()
prtMat = putStrLn .@ showMat

-- | show options for 'Mat'
data ShowOpts = ShowOpts
  { smIndent0 :: !Int
  -- ^ first indentation
  , smIndentN :: !Int
  -- ^ every subsequent indentation
  , smDivvy :: !Bool
  -- ^ split out into 'Vec' and 'Mat2' otherwise lump everything into 'Mat'
  , smInline1D :: !Bool
  -- ^ inline vector: large impact to output
  , smInlineNewLineEof :: !Bool
  -- ^ newline after each inlined vector: large impact to output
  , smOtherNewLineEof :: !Bool
  -- ^ newline after each except if inlined:large impact to output
  }
  deriving stock (Show, Eq, Ord)

-- | default show options for 'Mat'
defShowOpts :: ShowOpts
defShowOpts =
  ShowOpts
    { smIndent0 = 2
    , smIndentN = 0
    , smDivvy = True
    , smInline1D = True
    , smInlineNewLineEof = False
    , smOtherNewLineEof = True
    }

addNewline :: NonEmpty Pos -> ShowOpts -> Bool
addNewline (_ :| ns) opts =
  if null ns && smInline1D opts
    then smInlineNewLineEof opts
    else smOtherNewLineEof opts

instance (Show a, ShowMatC ns, NS ns) => Show (Mat ns a) where
  show = showMat defShowOpts

-- | show a matrix
showMat :: forall ns a. (ShowMatC ns, Show a) => ShowOpts -> Mat ns a -> String
showMat opts w@(Mat _ (n :| ns)) =
  let s = showMatC opts w
      zs = L.intercalate "\n" s
      ret = case (smDivvy opts, ns) of
        (True, []) -> "Vec@" ++ show (unP n) ++ bool "\n" " " (smInline1D opts)
        (True, [m]) -> "Mat2@" ++ show (unP n, unP m) ++ "\n"
        (_, _) -> "Mat@" ++ show (fromPositives (n : ns)) ++ "\n"
   in ret ++ zs ++ bool mempty "\n" (addNewline (n :| ns) opts)

-- | class with methods to convert to and from Mat using nested structures
class ShowMatC ns where
  -- | show a matrix
  showMatC :: Show a => ShowOpts -> Mat ns a -> [String]
  showMatC = showMatC' 1 1

  showMatC' :: Show a => Int -> Int -> ShowOpts -> Mat ns a -> [String]

instance GL.TypeError ( 'GL.Text "ShowMatC '[]: empty indices") => ShowMatC '[] where
  showMatC' = compileError "ShowMatC '[]:showMatC'"

instance ShowMatC '[n] where
  showMatC' i j _ (Mat v _) =
    let ret0 = show (V.toList v)
     in L.lines $ ret0 ++ if i == j then mempty else ","

instance ShowMatC (m ': ns) => ShowMatC (n ': m ': ns) where
  showMatC' i j opts w@(Mat _ (n :| _)) =
    let xs = toListMat $ rows w
        zz = replicate (3 + smIndent0 opts) ' ' -- 3 == length of "],["
        f s = [replicate (smIndent0 opts) ' ' ++ s]
        opts' = opts{smIndent0 = smIndentN opts}
        g i1 x1 = map (zz <>) (showMatC' (unP n) i1 opts' x1)
        s2 = concat $ zipWith g [1 ..] xs
     in case (i, j) of
          (1, 1) -> f "[" ++ s2 ++ f "]"
          (_, 1) -> f "[" ++ s2
          (_, _)
            | i == j -> f "],[" ++ s2 ++ f "]"
            | otherwise -> f "],[" ++ s2

-- | lens into row 1
class Row1 s a | s -> a where
  _r1 :: Lens' s a

-- | lens into row 2
class Row2 s a | s -> a where
  _r2 :: Lens' s a

-- | lens into row 3
class Row3 s a | s -> a where
  _r3 :: Lens' s a

-- | lens into row 4
class Row4 s a | s -> a where
  _r4 :: Lens' s a

-- | lens into row 5
class Row5 s a | s -> a where
  _r5 :: Lens' s a

-- | lens into row 6
class Row6 s a | s -> a where
  _r6 :: Lens' s a

-- | lens into row 7
class Row7 s a | s -> a where
  _r7 :: Lens' s a

-- | lens into row 8
class Row8 s a | s -> a where
  _r8 :: Lens' s a

-- | lens into row 9
class Row9 s a | s -> a where
  _r9 :: Lens' s a

-- | lens into row 10
class Row10 s a | s -> a where
  _r10 :: Lens' s a

-- | lens into the first row in a 2d or greater matrix
instance FinT 1 n => Row1 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r1 = _row @1

-- |  lens into the first element in a 1d matrix
instance FinT 1 n => Row1 (Vec n a) a where
  _r1 = _row @1

instance (FinT 2 n) => Row2 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r2 = _row @2

instance (FinT 2 n) => Row2 (Vec n a) a where
  _r2 = _row @2

instance (FinT 3 n) => Row3 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r3 = _row @3

instance (FinT 3 n) => Row3 (Vec n a) a where
  _r3 = _row @3

instance (FinT 4 n) => Row4 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r4 = _row @4

instance (FinT 4 n) => Row4 (Vec n a) a where
  _r4 = _row @4

instance (FinT 5 n) => Row5 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r5 = _row @5

instance (FinT 5 n) => Row5 (Vec n a) a where
  _r5 = _row @5

instance (FinT 6 n) => Row6 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r6 = _row @6

instance (FinT 6 n) => Row6 (Vec n a) a where
  _r6 = _row @6

instance (FinT 7 n) => Row7 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r7 = _row @7

instance (FinT 7 n) => Row7 (Vec n a) a where
  _r7 = _row @7

instance (FinT 8 n) => Row8 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r8 = _row @8

instance (FinT 8 n) => Row8 (Vec n a) a where
  _r8 = _row @8

instance (FinT 9 n) => Row9 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r9 = _row @9

instance (FinT 9 n) => Row9 (Vec n a) a where
  _r9 = _row @9

instance (FinT 10 n) => Row10 (Mat (n ': m ': ns) a) (Mat (m ': ns) a) where
  _r10 = _row @10

instance (FinT 10 n) => Row10 (Vec n a) a where
  _r10 = _row @10

-- | lens into column 1 of a matrix
_c1 :: FinT 1 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c1 = _col @1

-- | lens into column 2 of a matrix
_c2 :: FinT 2 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c2 = _col @2

-- | lens into column 3 of a matrix
_c3 :: FinT 3 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c3 = _col @3

-- | lens into column 4 of a matrix
_c4 :: FinT 4 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c4 = _col @4

-- | lens into column 5 of a matrix
_c5 :: FinT 5 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c5 = _col @5

-- | lens into column 6 of a matrix
_c6 :: FinT 6 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c6 = _col @6

-- | lens into column 7 of a matrix
_c7 :: FinT 7 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c7 = _col @7

-- | lens into column 8 of a matrix
_c8 :: FinT 8 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c8 = _col @8

-- | lens into column 9 of a matrix
_c9 :: FinT 9 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c9 = _col @9

-- | lens into column 10 of a matrix
_c10 :: FinT 10 m => Lens' (Mat (n ': (m : ns)) a) (Mat (n ': ns) a)
_c10 = _col @10

-- | marker representing the last value in a 1d matrix ie singleton
data Eof1 = Eof1 deriving stock (Show, Eq, Generic)

-- | marker representing the last row in a nd matrix ie singleton
data EofN = EofN deriving stock (Show, Eq, Generic)

type ConsMatCTA :: [Nat] -> Type -> Type
type family ConsMatCTA ns a where
  ConsMatCTA '[] _ = GL.TypeError ( 'GL.Text "ConsMatCTA '[]: empty indices")
  ConsMatCTA '[1] a = a
  ConsMatCTA '[_] a = a
  ConsMatCTA (1 ': m ': ns) a = Mat (m ': ns) a
  ConsMatCTA (_ ': m ': ns) a = Mat (m ': ns) a

type ConsMatCTB :: [Nat] -> Type -> Type
type family ConsMatCTB ns a where
  ConsMatCTB '[] _ = GL.TypeError ( 'GL.Text "ConsMatCTB '[]: empty indices")
  ConsMatCTB '[1] _ = Eof1
  ConsMatCTB '[n] a = Vec (n GN.- 1) a
  ConsMatCTB (1 ': _ ': _) _ = EofN
  ConsMatCTB (n ': m ': ns) a = Mat ((n GN.- 1) ': m ': ns) a

-- | iso and lenses to uncons a matrix
type ConsMatC :: [Nat] -> Type -> Type -> Constraint
class ConsMatC ns a b where
  consMat ::
    Iso
      (Mat ns a)
      (Mat ns b)
      (ConsMatCTA ns a, ConsMatCTB ns a)
      (ConsMatCTA ns b, ConsMatCTB ns b)
  headMat :: a ~ b => Lens' (Mat ns a) (ConsMatCTA ns a)
  headMat = consMat . _Fst

  tailMat :: a ~ b => Lens' (Mat ns a) (ConsMatCTB ns a)
  tailMat = consMat . _Snd

instance
  {-# OVERLAPPING #-}
  ( ConsMatCTA '[1] a ~ a
  , ConsMatCTA '[1] b ~ b
  , ConsMatCTB '[1] a ~ Eof1
  , ConsMatCTB '[1] b ~ Eof1
  ) =>
  ConsMatC '[1] a b
  where
  consMat = iso (\m -> (V.head (mVec m), Eof1)) (\(a, Eof1) -> se1 a)
instance
  {-# OVERLAPPABLE #-}
  ( ConsMatCTA '[n] a ~ a
  , ConsMatCTA '[n] b ~ b
  , ConsMatCTB '[n] a ~ Vec (n GN.- 1) a
  , ConsMatCTB '[n] b ~ Vec (n GN.- 1) b
  ) =>
  ConsMatC '[n] a b
  where
  consMat =
    iso
      ( \(Mat v0 (sn :| ps)) ->
          let n = frp $ predP sn
           in case V.uncons v0 of -- stay within Vector
                Nothing -> programmError "consMat '[1 GN.+ n]: no data"
                Just (a, v) -> (a, MatIU v (n :| ps))
      )
      (\(a, Mat v (p :| ps)) -> MatIU (V.cons a v) (succP p :| ps))

instance
  {-# OVERLAPPING #-}
  ( ConsMatCTA (1 ': m ': ns) a ~ Mat (m ': ns) a
  , ConsMatCTA (1 ': m ': ns) b ~ Mat (m ': ns) b
  , ConsMatCTB (1 ': m ': ns) a ~ EofN
  , ConsMatCTB (1 ': m ': ns) b ~ EofN
  ) =>
  ConsMatC (1 ': n1 ': ns) a b
  where
  consMat =
    iso
      ( \(Mat v (_ :| ps)) ->
          case ps of
            m : ns -> (MatIU v (m :| ns), EofN)
            [] -> programmError "consMat (1 ': m ': ns): missing indices"
      )
      (\(Mat v ps, EofN) -> MatIU v (_1P N.<| ps))

instance
  {-# OVERLAPPING #-}
  ( ConsMatCTA (n ': m ': ns) a ~ Mat (m ': ns) a
  , ConsMatCTA (n ': m ': ns) b ~ Mat (m ': ns) b
  , ConsMatCTB (n ': m ': ns) a ~ Mat ((n GN.- 1) ': m ': ns) a
  , ConsMatCTB (n ': m ': ns) b ~ Mat ((n GN.- 1) ': m ': ns) b
  ) =>
  ConsMatC (n ': m ': ns) a b
  where
  consMat =
    iso
      ( \(Mat v (sn :| ps)) ->
          case ps of
            m : ns ->
              let n = frp $ predP sn
                  ps1 = m :| ns
                  ps2 = n :| (m : ns)
                  (v1, v2) = V.splitAt (productPInt ps1) v
               in ( MatIU v1 ps1
                  , MatIU v2 ps2
                  )
            [] -> programmError "consMat:(1 GN.+ n ': m ': ns): missing indices"
      )
      (\(Mat v1 _, Mat v2 (p2 :| ps2)) -> MatIU (v1 <> v2) (succP p2 :| ps2))

-- | iso and lenses to unsnoc a matrix
type SnocMatC :: [Nat] -> Type -> Type -> Constraint
class SnocMatC ns a b where
  snocMat ::
    Iso
      (Mat ns a)
      (Mat ns b)
      (ConsMatCTB ns a, ConsMatCTA ns a)
      (ConsMatCTB ns b, ConsMatCTA ns b)

  initMat :: a ~ b => Lens' (Mat ns a) (ConsMatCTB ns a)
  initMat = snocMat . _Fst

  lastMat :: a ~ b => Lens' (Mat ns a) (ConsMatCTA ns a)
  lastMat = snocMat . _Snd

instance {-# OVERLAPPING #-} SnocMatC '[1] a b where
  snocMat =
    iso
      (\m -> (Eof1, V.last (mVec m)))
      (\(Eof1, a) -> MatIU (V.singleton a) (_1P :| []))
instance
  {-# OVERLAPPABLE #-}
  ( ConsMatCTB '[n] a ~ Vec (n GN.- 1) a
  , ConsMatCTB '[n] b ~ Vec (n GN.- 1) b
  ) =>
  SnocMatC '[n] a b
  where
  snocMat =
    iso
      ( \(Mat v0 (sn :| ps)) ->
          let n = frp $ predP sn
           in case V.unsnoc v0 of
                Nothing -> programmError "snocMat '[1 GN.+ n]: no data"
                Just (v, a) -> (MatIU v (n :| ps), a)
      )
      (\(Mat v (p :| ps), a) -> MatIU (V.snoc v a) (succP p :| ps))

instance {-# OVERLAPPING #-} SnocMatC (1 ': n1 ': ns) a b where
  snocMat =
    iso
      ( \(Mat v (_ :| ps)) ->
          case ps of
            m : ns ->
              (EofN, MatIU v (m :| ns))
            [] -> programmError "snocMat '[1 GN.+ n]: missing indices"
      )
      (\(EofN, Mat v ps) -> MatIU v (_1P N.<| ps))

instance
  {-# OVERLAPPABLE #-}
  ( ConsMatCTB (n ': m ': ns) a ~ Mat ((n GN.- 1) ': m ': ns) a
  , ConsMatCTB (n ': m ': ns) a ~ Mat ((n GN.- 1) ': m ': ns) b
  ) =>
  SnocMatC (n ': m ': ns) a b
  where
  snocMat =
    iso
      ( \(Mat v (sn :| ps)) ->
          case ps of
            m : ns ->
              let n = frp $ predP sn
                  ps1 = m :| ns
                  ps2 = n :| (m : ns)
                  (v2, v1) = V.splitAt (productPInt ps2) v
               in ( MatIU v2 ps2
                  , MatIU v1 ps1
                  )
            [] -> programmError "snocMat:(1 GN.+ n ': m ': ns): missing indices"
      )
      (\(Mat v1 (p1 :| ps1), Mat v2 _) -> MatIU (v1 <> v2) (succP p1 :| ps1))

-- | construct a new matrix based on a 1d matrix of row witnesses
rowsToMat ::
  forall x n m ns a.
  Vec x (Fin n) ->
  Mat (n ': m ': ns) a ->
  Mat (x ': m ': ns) a
rowsToMat w1@(Mat _ (x :| _)) w2@(Mat _ (_ :| ps)) =
  MatIU (V.concat $ toListMat $ fmap (\fn -> mVec (indexRow fn w2)) w1) (x :| ps)

-- | get a row from a matrix using a concrete index see '_row''
indexRow :: Fin n -> Mat (n ': m ': ns) a -> Mat (m ': ns) a
indexRow (Fin (Pos i) _n) (Mat v (_ :| ps)) =
  case ps of
    m : ns ->
      let s = (i - 1) * len
          len = productPInt (m :| ns)
       in MatIU (V.slice s len v) (m :| ns)
    [] -> programmError "indexRow: missing indices"

-- | 'Data.List.scanr' for a vector
scanrVec :: forall n a b. (a -> b -> b) -> b -> Vec n a -> Vec (n GN.+ 1) b
scanrVec f c (Mat v (p :| ps)) =
  MatIU (V.scanr' f c v) (succP p :| ps)

-- | 'Data.List.scanl'' for a vector
scanlVec :: forall n a b. (b -> a -> b) -> b -> Vec n a -> Vec (n GN.+ 1) b
scanlVec f c (Mat v (p :| ps)) =
  MatIU (V.scanl' f c v) (succP p :| ps)

{- | @see 'Data.Vector.postscanr''
 concrete version of 'Primus.Fold.postscanr
-}
postscanrMat :: forall ns a b. (a -> b -> b) -> b -> Mat ns a -> Mat ns b
postscanrMat f c (Mat v ps) =
  MatIU (V.postscanr' f c v) ps

{- | @see 'Data.Vector.postscanl''
 concrete version of 'Primus.Fold.postscanl'
-}
postscanlMat :: forall ns a b. (b -> a -> b) -> b -> Mat ns a -> Mat ns b
postscanlMat f c (Mat v ps) =
  MatIU (V.postscanl' f c v) ps

-- | matrix of dimension 1
dim1 :: Vec n a -> Vec n a
dim1 = id

-- | matrix of dimension 2
dim2 :: Mat2 n m a -> Mat2 n m a
dim2 = id

-- | matrix of dimension 3
dim3 :: Mat '[n, m, p] a -> Mat '[n, m, p] a
dim3 = id

-- | matrix of dimension 4
dim4 :: Mat '[n, m, p, q] a -> Mat '[n, m, p, q] a
dim4 = id

-- | matrix of dimension 5
dim5 :: Mat '[n, m, p, q, r] a -> Mat '[n, m, p, q, r] a
dim5 = id

-- | matrix of dimension 6
dim6 :: Mat '[n, m, p, q, r, s] a -> Mat '[n, m, p, q, r, s] a
dim6 = id

-- | matrix of dimension 7
dim7 :: Mat '[n, m, p, q, r, s, t] a -> Mat '[n, m, p, q, r, s, t] a
dim7 = id

-- | matrix of dimension 8
dim8 :: Mat '[n, m, p, q, r, s, t, u] a -> Mat '[n, m, p, q, r, s, t, u] a
dim8 = id

-- | matrix of dimension 9
dim9 :: Mat '[n, m, p, q, r, s, t, u, v] a -> Mat '[n, m, p, q, r, s, t, u, v] a
dim9 = id

-- | matrix of dimension 10
dim10 :: Mat '[n, m, p, q, r, s, t, u, v, w] a -> Mat '[n, m, p, q, r, s, t, u, v, w] a
dim10 = id
