{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Cybus.FinMat
Description : fixed-sized indices for a matrix
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Cybus.FinMat (
  type FinMat,
  fmPos,
  fmNS,
  pattern FinMat,
  pattern FinMatU,
  mkFinMat,
  mkFinMatC,
  toFinMatFromPos,
  FinMatC (..),
  finMatToNonEmpty,
  nonEmptyToFinMat,
  nonEmptyToFinMat',

  -- * read/show methods
  showFinMat,
  readFinMatP,
  readFinMat,
  showFinMat',

  -- * constructors

  -- * miscellaneous
  NSC (..),
  NSRangeC,
  _finMatFin,
  finMatFinSet,
  finMatFinGet,
  relPos,

  -- * lens into indices of matrix
  _i1,
  _i2,
  _i3,
  _i4,
  _i5,
  _i6,
  _i7,
  _i8,
  _i9,
  _i10,
) where

import Control.DeepSeq
import Cybus.Fin
import Cybus.NatHelper
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.Semigroup.Foldable
import Data.These
import GHC.Enum
import GHC.Generics (Generic, Generic1)
import GHC.Read (readPrec)
import GHC.Stack
import qualified GHC.TypeLits as GL
import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as GN
import Primus.Enum
import Primus.Error
import Primus.Extra
import Primus.Lens
import Primus.NonEmpty
import Primus.Num1
import qualified Primus.TypeLevel as TP (Len1T, pnat)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC

-- | definition of the indices of a matrix
type FinMat :: NonEmpty Nat -> Type
data FinMat ns = FinMatUnsafe !Int !(NonEmpty Pos)
  deriving stock (Eq, Ord, Generic, Generic1)
  deriving anyclass (NFData)

-- | accessor for the relative position within a matrix
fmPos :: FinMat ns -> Int
fmPos (FinMatUnsafe i _) = i

-- | accessor for the indices of a matrix
fmNS :: FinMat ns -> NonEmpty Pos
fmNS (FinMatUnsafe _ ns) = ns

-- | readonly pattern synonym for finmatrix
{-# COMPLETE FinMat #-}

pattern FinMat ::
  forall (ns :: NonEmpty Nat).
  Int ->
  NonEmpty Pos ->
  FinMat ns
pattern FinMat i ps <- FinMatUnsafe i ps

{-# COMPLETE FinMatU #-}

-- | pattern synonym for validating the finmatrix before construction but uses an extra 'NSC' constraint to check "ns"
pattern FinMatU ::
  forall (ns :: NonEmpty Nat).
  (HasCallStack, NSC ns) =>
  Int ->
  NonEmpty Pos ->
  FinMat ns
pattern FinMatU i ps <-
  FinMatUnsafe i ps
  where
    FinMatU = frp .@ mkFinMatC -- dont change this: frp is necessary else breaking the system

-- | create a FinMat value level "i" and "ns" values and validate that "i" is in range
mkFinMat :: Int -> NonEmpty Pos -> Either String (FinMat ns)
mkFinMat i ps = lmsg "mkFinMat" $ do
  let tot = productPInt ps
  if
      | i < 0 -> Left $ "cant be less than 0: i=" ++ show i
      | i >= tot -> Left $ "is too large: maximum is " ++ show (tot - 1) ++ " but found " ++ show i
      | otherwise -> pure (FinMatUnsafe i ps)

-- | create a FinMat value level "i" and "ns" values and validate against expected "ns"
mkFinMatC :: forall ns. NSC ns => Int -> NonEmpty Pos -> Either String (FinMat ns)
mkFinMatC i ps = do
  let ns = fromNSP @ns
  if ns == ps
    then mkFinMat i ps
    else Left $ "mkFinMatC: invalid indices: typelevel " ++ show (fromPositives ns) ++ " /= " ++ show (fromPositives ps)

-- | create a FinMat using a relative type level index
toFinMatFromPos :: forall (i :: Nat) ns. (NSC ns, i <! Product1T ns) => FinMat ns
toFinMatFromPos = FinMatU (TP.pnat @i) (fromNSP @ns)

-- | convert type level indices into a FinMat
class FinMatC is ns where
  finMatC :: FinMat ns

instance (NSC is, NSC ns, FinMatT is ns 1 is ns) => FinMatC is ns where
  finMatC = frp $ nonEmptyToFinMat' (fromNSP @is) (fromNSP @ns)

type FinMatT :: NonEmpty Nat -> NonEmpty Nat -> Nat -> NonEmpty Nat -> NonEmpty Nat -> Constraint
type family FinMatT is0 ns0 ind is ns where
  FinMatT _is0 _ns0 ind (i ':| '[]) (n ':| '[]) =
    FinWithMessageT ( 'GL.Text " at index " 'GL.:<>: 'GL.ShowType ind) i n
  FinMatT is0 ns0 ind (i ':| i' ': is) (n ':| n' ': ns) =
    (FinWithMessageT ( 'GL.Text " at index=" 'GL.:<>: 'GL.ShowType ind) i n, FinMatT is0 ns0 (ind GN.+ 1) (i' ':| is) (n' ':| ns))
  FinMatT is0 ns0 _ind (_ ':| _ ': _) (_ ':| '[]) =
    GL.TypeError
      ( 'GL.Text "too many indices: length is > length ns:"
          'GL.:<>: 'GL.Text " found "
          'GL.:<>: 'GL.ShowType (TP.Len1T is0)
          'GL.:<>: 'GL.Text " expected "
          'GL.:<>: 'GL.ShowType (TP.Len1T ns0)
      )
  FinMatT is0 ns0 _ind (_ ':| '[]) (_ ':| _ ': _) =
    GL.TypeError
      ( 'GL.Text "not enough indices: length is < length ns: "
          'GL.:<>: 'GL.Text " found "
          'GL.:<>: 'GL.ShowType (TP.Len1T is0)
          'GL.:<>: 'GL.Text " expected "
          'GL.:<>: 'GL.ShowType (TP.Len1T ns0)
      )

-- | convert a FinMat into a list of indices
finMatToNonEmpty :: forall ns. FinMat ns -> NonEmpty Pos
finMatToNonEmpty (FinMat i ns) = snd $ L.mapAccumR divModNextP i ns

-- | try to convert a list of indices into a FinMat
nonEmptyToFinMat :: forall ns. NSC ns => NonEmpty Pos -> Either String (FinMat ns)
nonEmptyToFinMat is = nonEmptyToFinMat' is (fromNSP @ns)

-- | try to convert a list of indices into a FinMat
nonEmptyToFinMat' :: NonEmpty Pos -> NonEmpty Pos -> Either String (FinMat ns)
nonEmptyToFinMat' is ns =
  lmsg "nonEmptyToFinMat" $
    let (lrs, mlr) = zipWithExtras1 g is ns
        g :: Pos -> Pos -> Either String (Pos, Pos)
        g x y =
          if x <= y
            then Right (x, y)
            else Left $ "outofbounds " ++ show (x, y)
     in case mlr of
          MLREqual -> case partitionEithersNE lrs of
            That xys -> Right $ frp $ mkFinMat (snd $ relPos xys) ns
            This es -> Left $ "This " ++ L.intercalate "\n" (N.toList es)
            These es as -> Left $ "These es=" ++ L.intercalate "\n" (N.toList es) ++ " as=" ++ show as
          MLRLeft{} -> Left $ "too many indices: expected " ++ show (unP (lengthP ns)) ++ " is=" ++ show is ++ " ns=" ++ show ns
          MLRRight{} -> Left $ "not enough indices: expected " ++ show (unP (lengthP ns)) ++ " is=" ++ show is ++ " ns=" ++ show ns

-- | find the relative position in a matrix index
relPos :: Foldable1 t => t (Pos, Pos) -> (Pos, Int)
relPos xys =
  let ret@(Pos a, b) = foldr (\(Pos x, y) (z, tot) -> (z *! y, (x - 1) * unP z + tot)) (_1P, 0) xys
   in if b >= a
        then programmError $ "relPos " ++ show ret
        else ret

instance NSC ns => Monoid (FinMat ns) where
  mempty = minBound

instance Semigroup (FinMat ns) where
  (<>) = max

instance NSC ns => Num (FinMat ns) where
  (+) = forceRight "(+)" .@ withOp2 (+)
  (-) = forceRight "(-)" .@ withOp2 (-)
  (*) = forceRight "(*)" .@ withOp2 (*)
  abs = id
  signum (FinMat i ns) = FinMatU (signum i) ns
  negate = normalError "Num (FinMat ns):negate is undefined"
  fromInteger i = forceRight "Num (FinMat ns):fromInteger" $ do
    if i < 0
      then Left "undefined for negative numbers"
      else do
        ii <- integerToIntSafe i
        mkFinMatC ii (fromNSP @ns)

instance NSC ns => Num1 (FinMat ns) where
  fromInteger1 (FinMat _ ns) i = do
    ii <- integerToIntSafe i
    mkFinMatC ii ns

instance NSC ns => Enum (FinMat ns) where
  toEnum = forceRight "Enum(FinMat ns):toEnum" . flip mkFinMatC (fromNSP @ns)
  fromEnum = fmPos
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance NSC ns => Bounded (FinMat ns) where
  minBound = FinMatU 0 (fromNSP @ns)
  maxBound = FinMatU (unP (fromNSTotalP @ns) - 1) (fromNSP @ns)

instance NSC ns => Read (FinMat ns) where
  readPrec = PC.readP_to_Prec (const readFinMatP)

-- | reader for 'FinMat'
readFinMat :: NSC ns => ReadS (FinMat ns)
readFinMat = P.readP_to_S readFinMatP

-- | reader for 'showFin'
readFinMatP :: forall ns. NSC ns => P.ReadP (FinMat ns)
readFinMatP = do
  P.skipSpaces
  (i, ns) <- (,) <$> pInt <* P.char '@' <*> pPositives '{' '}'
  either (const P.pfail) pure $ mkFinMatC @ns i ns

neToString :: NonEmpty Pos -> String
neToString = L.intercalate "," . map show . fromPositives

-- | pretty print FinMat
showFinMat :: FinMat ns -> String
showFinMat (FinMat i ns) =
  show i ++ "@{" ++ neToString ns ++ "}"

-- | more detailed pretty print FinMat
showFinMat' :: forall ns. FinMat ns -> String
showFinMat' w@(FinMat i ns) =
  show i ++ "@{" ++ neToString (finMatToNonEmpty w) ++ "|" ++ neToString ns ++ "}"

instance Show (FinMat ns) where
  show = showFinMat

-- | constrain i within the size of the indices ie "i >= 1 && i <= Length ns"
type NSRangeC :: Peano -> NonEmpty Nat -> Constraint
class NSRangeC i ns

instance NSRangeC ( 'S 'Z) (n ':| ns)
instance NSRangeC ( 'S i) (m ':| ns) => NSRangeC ( 'S ( 'S i)) (n ':| m ': ns)
instance
  GL.TypeError ( 'GL.Text "NSRangeC: index is larger than the number of matrix indices ns") =>
  NSRangeC ( 'S ( 'S i)) (n ':| '[])

instance
  GL.TypeError ( 'GL.Text "NSRangeC: zero is not a valid index: index must be one or greater") =>
  NSRangeC 'Z (n ':| ns)

-- | a lens for accessing the "i" index in a indices of FinMat
_finMatFin ::
  forall i n ns.
  (PosT i, NSRangeC (NatToPeanoT i) ns) =>
  Lens' (FinMat ns) (Fin n)
_finMatFin = lens (finMatFinGet @i @n @ns) (finMatFinSet @i @n @ns)

-- | set the 'Fin' at index "i" for the FinMat
finMatFinSet ::
  forall i n ns.
  (PosT i, NSRangeC (NatToPeanoT i) ns) =>
  FinMat ns ->
  Fin n ->
  FinMat ns
finMatFinSet fm@(FinMat _ ns) (Fin ind _) =
  let i = fromNP @i
      ps = finMatToNonEmpty fm
   in case setAt1 i ind ps of
        Nothing -> programmError $ "finMatFinSet: index out of bounds: index is " ++ show i
        Just ps1 -> frp $ nonEmptyToFinMat' ps1 ns

{- | get the 'Fin' at index "i" from FinMat
 must rely on FinMat to get "n at index i "which saves us pulling "n" from the typelevel ie we can omit PosT n
-}
finMatFinGet ::
  forall i n ns.
  (PosT i, NSRangeC (NatToPeanoT i) ns) =>
  FinMat ns ->
  Fin n
finMatFinGet fm@(FinMat _ ns) =
  let i = fromNP @i
      ps = finMatToNonEmpty fm
   in case (at1 i ps, at1 i ns) of
        (Nothing, _) -> programmError "finMatFinGet: invalid index!"
        (_, Nothing) -> programmError $ "finMatFinGet: FinMat is corrupt: doesnt have the index at " ++ show i ++ " " ++ show fm
        (Just p, Just n) -> frp $ mkFin p n

-- | lens for index 1
_i1 :: Lens' (FinMat (n ':| ns)) (Fin n)
_i1 = _finMatFin @1

-- | lens for index 2
_i2 :: Lens' (FinMat (n1 ':| n ': ns)) (Fin n)
_i2 = _finMatFin @2

-- | lens for index 3
_i3 :: Lens' (FinMat (n1 ':| n2 ': n ': ns)) (Fin n)
_i3 = _finMatFin @3

-- | lens for index 4
_i4 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n ': ns)) (Fin n)
_i4 = _finMatFin @4

-- | lens for index 5
_i5 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n4 ': n ': ns)) (Fin n)
_i5 = _finMatFin @5

-- | lens for index 6
_i6 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n4 ': n5 ': n ': ns)) (Fin n)
_i6 = _finMatFin @6

-- | lens for index 7
_i7 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n4 ': n5 ': n6 ': n ': ns)) (Fin n)
_i7 = _finMatFin @7

-- | lens for index 8
_i8 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n4 ': n5 ': n6 ': n7 ': n ': ns)) (Fin n)
_i8 = _finMatFin @8

-- | lens for index 9
_i9 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n4 ': n5 ': n6 ': n7 ': n8 ': n ': ns)) (Fin n)
_i9 = _finMatFin @9

-- | lens for index 10
_i10 :: Lens' (FinMat (n1 ':| n2 ': n3 ': n4 ': n5 ': n6 ': n7 ': n8 ': n9 ': n ': ns)) (Fin n)
_i10 = _finMatFin @10