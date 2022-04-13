{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Cybus.NatHelper
Description : Nat helper methods
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Cybus.NatHelper (
  -- * peano
  NatToPeanoT,
  PeanoToNatT,
  Peano (..),

  -- * arithmetic
  DiffT,
  DiffTC,
  FacT,
  type (<=!),
  type (<!),
  LTEQT,

  -- * matrix dimension synonyms
  D1,
  D2,
  D3,
  D4,
  D5,
  D6,
  D7,
  D8,
  D9,
  D10,

  -- * matrix helpers
  ProductT,
  NN,
  NN',
  ReverseT,
  ListTupleT,

  -- * list and nonempty conversions
  ValidateNestedListC (..),
  ValidateNestedNonEmptyC (..),
  validateNestedList,
  validateNestedNonEmpty,
  ValidateNestedListT,
  ValidateNestedNonEmptyT,
  nestedNonEmptyToList,
  nestedListToNonEmpty,
  NestedListC (..),
  NonEmptyNST,
  ListNST,
) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.Proxy
import qualified GHC.TypeLits as GL
import GHC.TypeNats (Nat,KnownNat)
import qualified GHC.TypeNats as GN
import Primus.Error
import Primus.Fold
import Primus.List
import Primus.NonEmpty
import Primus.One
import Primus.TypeLevel (FailUnless)

-- | get the factorial of a 'Nat'
type FacT :: Nat -> Nat
type family FacT x where
  FacT 0 = 1
  FacT 1 = 1
  FacT n = n GN.* FacT (n GN.- 1)

-- | constraint for ensuring that "i" <= "n"
type (<=!) :: Nat -> Nat -> Constraint
type i <=! n =
  ( FailUnless
      (i GN.<=? n)
      ( 'GL.Text "i>n"
          'GL.:<>: 'GL.Text ": i="
          'GL.:<>: 'GL.ShowType i
          'GL.:<>: 'GL.Text " n="
          'GL.:<>: 'GL.ShowType n
      )
  , PosC i
  )

-- | constraint for ensuring that "i" <= "n" with a custom error message
type LTEQT :: GL.ErrorMessage -> Nat -> Nat -> Constraint
type LTEQT msg i n =
  ( FailUnless
      (i GN.<=? n)
      ( 'GL.Text "i>n"
          'GL.:<>: 'GL.Text ": i="
          'GL.:<>: 'GL.ShowType i
          'GL.:<>: 'GL.Text " n="
          'GL.:<>: 'GL.ShowType n
          'GL.:<>: msg
      )
  , PosC i
  )

-- | constraint for ensuring that "i" <= "n"
type (<!) :: Nat -> Nat -> Constraint
type i <! n =
  ( FailUnless
      (i GN.+ 1 GN.<=? n)
      ( 'GL.Text "i>=n"
          'GL.:<>: 'GL.Text ": i="
          'GL.:<>: 'GL.ShowType i
          'GL.:<>: 'GL.Text " n="
          'GL.:<>: 'GL.ShowType n
      )
  , KnownNat i
  )

-- | constraint for positive numbers
type LTEQC :: Nat -> Nat -> Constraint
class (KnownNat i, KnownNat n) => LTEQC i n where
instance
  ( KnownNat i
  , KnownNat n
  , FailUnless
      (i GL.<=? n)
      ( 'GL.Text "LTEQC n: requires n >= i but found i="
          'GL.:<>: 'GL.ShowType i
          'GL.:<>: 'GL.Text " n="
          'GL.:<>: 'GL.ShowType n
      )
  ) =>
  LTEQC i n

-- | constraint for DiffC with better error messages
type DiffTC :: Nat -> Nat -> Nat -> Constraint
type DiffTC i j n = (i <=! j, j <=! n)

-- | find the number of N between "i" and "j" while ensuring i<=j and j<=n
type DiffT :: Nat -> Nat -> Nat -> Nat
type DiffT i j n = j GN.+ 1 GN.- i

-- | reverse a type level list
type ReverseT :: forall k. [k] -> [k]
type family ReverseT ns where
  ReverseT (n ': ns) = ReverseT' (n ': ns) '[]

-- | used by 'ReverseT'
type ReverseT' :: forall k. [k] -> [k] -> [k]
type family ReverseT' ns ret where
  ReverseT' '[] (r ': rs) = r ': rs
  ReverseT' (n ': ns) ret = ReverseT' ns (n ': ret)

-- | product of a type level list as a 'Nat'
type ProductT :: [Nat] -> Nat
type family ProductT ns where
  ProductT '[] = GL.TypeError ( 'GL.Text "ProductT: empty indices")
  ProductT '[n] = n
  ProductT (n ': n1 ': ns) = n GN.* ProductT (n1 ': ns)

-- | extracts the dimensions of a nested list
type ValidateNestedListT :: Type -> Peano
type family ValidateNestedListT x where
  ValidateNestedListT [x] = 'S (ValidateNestedListT x)
  ValidateNestedListT _ = 'S 'Z

-- | extracts the dimensions of a nested nonempty list
type ValidateNestedNonEmptyT :: Type -> Peano
type family ValidateNestedNonEmptyT x where
  ValidateNestedNonEmptyT (NonEmpty x) = 'S (ValidateNestedNonEmptyT x)
  ValidateNestedNonEmptyT _ = 'S 'Z

-- | validate that the nested nonempty list is consistent in size along all dimensions
validateNestedNonEmpty :: forall x. ValidateNestedNonEmptyC x (ValidateNestedNonEmptyT x) => x -> Either String (NonEmpty Pos)
validateNestedNonEmpty x = validateNestedNonEmptyC @x @(ValidateNestedNonEmptyT x) [] x []

-- | validate that the nested list is consistent in size along all dimensions
validateNestedList :: forall x. ValidateNestedListC x (ValidateNestedListT x) => x -> Either String (NonEmpty Pos)
validateNestedList x = validateNestedListC @x @(ValidateNestedListT x) [] x []

-- | extracts the dimensions of a nested nonempty list: doesnt allow empty dimensions
type ValidateNestedNonEmptyC :: Type -> Peano -> Constraint
class ValidateNestedNonEmptyC x y where
  validateNestedNonEmptyC :: [Pos] -> x -> [x] -> Either String (NonEmpty Pos)

instance GL.TypeError ( 'GL.Text "ValidateNestedNonEmptyC: not defined at 'Z") => ValidateNestedNonEmptyC x 'Z where
  validateNestedNonEmptyC = compileError "validateNestedNonEmptyC: ValidateNestedNonEmptyC x 'Z"
instance ValidateNestedNonEmptyC x ( 'S 'Z) where
  validateNestedNonEmptyC ixes _ _ =
    case ixes of
      i : is -> Right (i :| is)
      [] -> programmError "ValidateNestedNonEmptyC: ('S 'Z): empty list of indices"
instance ValidateNestedNonEmptyC x ( 'S zs) => ValidateNestedNonEmptyC (NonEmpty x) ( 'S ( 'S zs)) where
  validateNestedNonEmptyC ixes x@(n :| ns) xs =
    let cs = map clOrdering $ compareLengths x xs
     in if all (Just EQ ==) cs
          then
            let zs = ns <> concatMap N.toList xs
             in validateNestedNonEmptyC @x @( 'S zs) (ixes `snocL` lengthP x) n zs
          else Left $ "validateNestedNonEmptyC: lengths=" ++ show (map length (x : xs)) ++ " ixes=" ++ show (map unP ixes)

-- | extracts the dimensions of a nested list: doesnt allow empty dimensions
type ValidateNestedListC :: Type -> Peano -> Constraint
class ValidateNestedListC x y where
  validateNestedListC :: [Pos] -> x -> [x] -> Either String (NonEmpty Pos)

instance GL.TypeError ( 'GL.Text "ValidateNestedListC: not defined at 0") => ValidateNestedListC x 'Z where
  validateNestedListC = compileError "validateNestedListC: ValidateNestedListC x 'Z"
instance ValidateNestedListC x ( 'S 'Z) where
  validateNestedListC ixes _ _ =
    case ixes of
      i : is -> Right (i :| is)
      [] -> programmError "ValidateNestedListC: ('S 'Z): empty list of indices"
instance ValidateNestedListC x ( 'S n) => ValidateNestedListC [x] ( 'S ( 'S n)) where
  validateNestedListC ixes [] _ = Left $ "validateNestedListC: ixes=" ++ show ixes ++ ":no data!"
  validateNestedListC ixes x@(n : ns) xs =
    let cs = map clOrdering $ compareLengths x xs
     in if all (Just EQ ==) cs
          then
            let zs = ns <> concat xs
             in validateNestedListC @x @( 'S n) (ixes `snocL` lengthP (n :| ns)) n zs
          else Left $ "validateNestedListC: lengths=" ++ show (map length (x : xs)) ++ " ixes=" ++ show ixes

-- | peano numbers for converting between 'Nat' and peano
data Peano = Z | S !Peano deriving stock (Ord, Show, Eq)

-- | convert Nat to Peano
type NatToPeanoT :: Nat -> Peano
type family NatToPeanoT n where
  NatToPeanoT 0 = 'Z
  NatToPeanoT n = 'S (NatToPeanoT (n GN.- 1))

-- | convert Peano to Nat
type PeanoToNatT :: Peano -> Nat
type family PeanoToNatT n where
  PeanoToNatT 'Z = 0
  PeanoToNatT ( 'S n) = 1 GN.+ PeanoToNatT n

-- | convert a matrix index into nested lists
type ListNST :: [Nat] -> Type -> Type
type family ListNST ns a where
  ListNST '[] _ = GL.TypeError ( 'GL.Text "ListNST: empty indices")
  ListNST '[_] a = [a]
  ListNST (_ ': n1 ': ns) a = [ListNST (n1 ': ns) a]

-- | convert a matrix index into nested lists
type NonEmptyNST :: [Nat] -> Type -> Type
type family NonEmptyNST ns a where
  NonEmptyNST '[] _ = GL.TypeError ( 'GL.Text "NonEmptyNST: empty indices")
  NonEmptyNST '[_] a = NonEmpty a
  NonEmptyNST (_ ': n1 ': ns) a = NonEmpty (NonEmptyNST (n1 ': ns) a)

-- | convert a nested nonempty list into a nested list
nestedNonEmptyToList :: forall ns a. NestedListC ns => NonEmptyNST ns a -> Either String (ListNST ns a)
nestedNonEmptyToList = nestedNonEmptyToListC @ns (Proxy @a)

-- | convert a nested list into a nested nonempty list
nestedListToNonEmpty :: forall ns a. NestedListC ns => ListNST ns a -> Either String (NonEmptyNST ns a)
nestedListToNonEmpty = nestedListToNonEmptyC @ns @_ @a Proxy

-- | methods for working with nested lists
type NestedListC :: [Nat] -> Constraint
class NestedListC ns where
  -- | convert a nested list to a nested nonempty list
  nestedListToNonEmptyC :: proxy a -> ListNST ns a -> Either String (NonEmptyNST ns a)

  -- | convert a nested nonempty list to a nested list
  nestedNonEmptyToListC :: proxy a -> NonEmptyNST ns a -> Either String (ListNST ns a) -- need a proxy to make it work and find the correct 'a'

  flattenNestedListC :: proxy a -> ListNST ns a -> Either String [a]

instance GL.TypeError ( 'GL.Text "NestedListC '[]: empty indices") => NestedListC '[] where
  nestedListToNonEmptyC = compileError "NestedListC '[]:nestedListToNonEmptyC"
  nestedNonEmptyToListC = compileError "NestedListC '[]:nestedNonEmptyToListC"
  flattenNestedListC = compileError "NestedListC '[]:flattenNestedListC"

instance PosC n => NestedListC '[n] where
  nestedListToNonEmptyC _ = \case
    [] -> Left "nestedListToNonEmptyC 'SZ no data"
    x : xs -> lmsg "nestedListToNonEmptyC 'SZ" $ lengthExact1 (fromNP @n) (x :| xs)
  nestedNonEmptyToListC _ lst = N.toList <$> lmsg "nestedNonEmptyToListC 'SZ" (lengthExact1 (fromNP @n) lst)
  flattenNestedListC _ = \case
    [] -> Left "flattenNestedListC 'SZ no data"
    x : xs -> lmsg "flattenNestedListC 'SZ" $ lengthExact (fromN @n) (x : xs)

instance (PosC n, NestedListC (n1 ': ns)) => NestedListC (n ': n1 ': ns) where
  nestedListToNonEmptyC p = \case
    [] -> Left "nestedListToNonEmptyC 'SS no data"
    x : xs -> do
      ys <- lmsg "nestedListToNonEmptyC 'SS" $ lengthExact1 (fromNP @n) (x :| xs)
      traverse (nestedListToNonEmptyC @(n1 ': ns) p) ys
  nestedNonEmptyToListC p lst = do
    xs <- lmsg "nestedNonEmptyToListC 'SS" $ lengthExact1 (fromNP @n) lst
    N.toList <$> traverse (nestedNonEmptyToListC @(n1 ': ns) p) xs
  flattenNestedListC p = \case
    [] -> Left "flattenNestedListC 'SS no data"
    x : xs -> do
      ys <- lmsg "flattenNestedListC 'SS" $ lengthExact (fromN @n) (x : xs)
      concat <$> traverse (flattenNestedListC @(n1 ': ns) p) ys

-- mapM_ (putStrLn . genListTupleT) [2..20]  -- to generate from two onwards

-- | translates a type of size "n" to a tuple of size "n"
type ListTupleT :: Nat -> Type -> Type
type family ListTupleT n a = result | result -> n a where
  ListTupleT 1 a = One a
  ListTupleT 2 a = (a, a)
  ListTupleT 3 a = (a, a, a)
  ListTupleT 4 a = (a, a, a, a)
  ListTupleT 5 a = (a, a, a, a, a)
  ListTupleT 6 a = (a, a, a, a, a, a)
  ListTupleT 7 a = (a, a, a, a, a, a, a)
  ListTupleT 8 a = (a, a, a, a, a, a, a, a)
  ListTupleT 9 a = (a, a, a, a, a, a, a, a, a)
  ListTupleT 10 a = (a, a, a, a, a, a, a, a, a, a)
  ListTupleT 11 a = (a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 12 a = (a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 13 a = (a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 14 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 15 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 16 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 17 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 18 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 19 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  ListTupleT 20 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

-- | generates a list of indices using each digit of the given 'Nat'
type NN :: Nat -> [Nat]
type NN n = NN' '[] n

-- | generates a list of indices using the individual digits of the given 'Nat'
type NN' :: [Nat] -> Nat -> [Nat]
type family NN' ns n where
  NN' ns 0 = ns
  NN' ns n = NN' (GN.Mod n 10 ': ns) (GN.Div n 10)

-- | matrix dimension of degree 1
type D1 :: Nat -> [Nat]
type D1 a = '[a]

-- | matrix dimension of degree 2
type D2 :: Nat -> Nat -> [Nat]
type D2 a b = '[a, b]

-- | matrix dimension of degree 3
type D3 :: Nat -> Nat -> Nat -> [Nat]
type D3 a b c = '[a, b, c]

-- | matrix dimension of degree 4
type D4 :: Nat -> Nat -> Nat -> Nat -> [Nat]
type D4 a b c d = '[a, b, c, d]

-- | matrix dimension of degree 5
type D5 :: Nat -> Nat -> Nat -> Nat -> Nat -> [Nat]
type D5 a b c d e = '[a, b, c, d, e]

-- | matrix dimension of degree 6
type D6 :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> [Nat]
type D6 a b c d e f = '[a, b, c, d, e, f]

-- | matrix dimension of degree 7
type D7 :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> [Nat]
type D7 a b c d e f g = '[a, b, c, d, e, f, g]

-- | matrix dimension of degree 8
type D8 :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> [Nat]
type D8 a b c d e f g h = '[a, b, c, d, e, f, g, h]

-- | matrix dimension of degree 9
type D9 :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> [Nat]
type D9 a b c d e f g h i = '[a, b, c, d, e, f, g, h, i]

-- | matrix dimension of degree 10
type D10 :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> [Nat]
type D10 a b c d e f g h i j = '[a, b, c, d, e, f, g, h, i, j]
