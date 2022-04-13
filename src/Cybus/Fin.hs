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
Module      : Cybus.Fin
Description : finite numbers
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3

used for single indexes into a 'Cybus.Mat.Mat' or 'Cybus.FinMat.FinMat'
-}
module Cybus.Fin (
  type Fin,
  fnPos,
  fnN,
  pattern Fin,
  pattern FinU,
  FinC (..),
  FinWithMessageC,

  -- * read/show methods
  showFin,
  readFinP,
  readFin,

  -- * constructors
  mkFinC,
  mkFin,
  fin,
  finP,

  -- * fin indexes
  _F1,
  _F2,
  _F3,
  _F4,
  _F5,
  _F6,
  _F7,
  _F8,
  _F9,
  _F10,
  _F11,
  _F12,
  _F13,
  _F14,
  _F15,
  _F16,
  _F17,
  _F18,
  _F19,
  _F20,
) where

import Control.DeepSeq
import Control.Monad
import Cybus.NatHelper
import Data.Kind
import Data.Pos
import GHC.Enum
import GHC.Generics (Generic, Generic1)
import GHC.Read (readPrec)
import GHC.Stack
import qualified GHC.TypeLits as GL
import GHC.TypeNats (Nat)
import Primus.Enum
import Primus.Error
import Primus.Num1
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC

-- | definition of the Finite type
type Fin :: Nat -> Type
data Fin n = Fin' !Pos !Pos
  deriving stock (Eq, Ord, Generic, Generic1)
  deriving anyclass (NFData)

-- | accessor for the index position within a 'Fin'
fnPos :: Fin n -> Pos
fnPos (Fin' i _) = i

-- | accessor for the maximum size within a 'Fin'
fnN :: Fin n -> Pos
fnN (Fin' _ n) = n

-- | readonly pattern synonym for fin
{-# COMPLETE Fin #-}

pattern Fin ::
  forall (n :: Nat).
  Pos ->
  Pos ->
  Fin n
pattern Fin i n <- Fin' i n

{-# COMPLETE FinU #-}

-- | pattern synonym for validating the fin before construction with a PosC constraint for validating at the typelevel
pattern FinU ::
  forall (n :: Nat).
  (HasCallStack, PosC n) =>
  Pos ->
  Pos ->
  Fin n
pattern FinU i n <-
  Fin' i n
  where
    FinU = frp .@ mkFinC

-- | create a 'Fin' value level "i" and "n" values and validate that "i" is in range
mkFin :: Pos -> Pos -> Either String (Fin n)
mkFin p n = lmsg "mkFin" $ do
  if p <= n
    then pure (Fin' p n)
    else Left $ show p ++ " is too large: maximum is " ++ show n

-- | create a 'Fin' value level "i" and "n" values and validate against expected "n"
mkFinC :: forall n. PosC n => Pos -> Pos -> Either String (Fin n)
mkFinC p n = do
  let n' = fromNP @n
  if n == n'
    then mkFin p n
    else Left $ "mkFinC: " ++ show n ++ " /= " ++ show n' ++ " at typelevel"

-- | convenience function for conversion from 'Int' to 'Fin'
fin :: PosC n => Int -> Either String (Fin n)
fin = finP <=< eitherPos

-- | convenience function for conversion from 'Pos' to 'Fin'
finP :: forall n. PosC n => Pos -> Either String (Fin n)
finP = flip mkFinC (fromNP @n)

instance PosC n => Monoid (Fin n) where
  mempty = minBound

instance Semigroup (Fin n) where
  (<>) = max

-- PosC only needed for fromInteger
instance PosC n => Num (Fin n) where
  (+) = forceRight "(+)" .@ withOp2 (+)
  (-) = forceRight "(-)" .@ withOp2 (-)
  (*) = forceRight "(*)" .@ withOp2 (*)
  abs = id
  signum (Fin _ n) = FinU _1P n
  negate = normalError "Num (Fin n):negate is undefined"
  fromInteger i = forceRight "Num (Fin n):fromInteger" $ do
    ii <- integerToIntSafe (i + 1)
    k <- eitherPos ii
    mkFinC k (fromNP @n)

instance PosC n => Num1 (Fin n) where
  signum1 = fmap signum -- have to override as 1 is Fin 2 (there is no zero)

instance PosC n => Enum (Fin n) where
  toEnum i = forceRight "Enum(Fin n):toEnum" $ do
    p <- eitherPos (i + 1)
    mkFinC p (fromNP @n)
  fromEnum = subtract 1 . unP . fnPos
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance PosC n => Bounded (Fin n) where
  minBound = FinU _1P (fromNP @n)
  maxBound = FinU (fromNP @n) (fromNP @n)

-- | pretty print 'Fin'
showFin :: Fin n -> String
showFin (Fin (Pos i) (Pos n)) = "Fin" ++ show (i, n)

instance PosC n => Read (Fin n) where
  readPrec = PC.readP_to_Prec (const readFinP)

-- | reader for 'Fin'
readFin :: PosC n => ReadS (Fin n)
readFin = P.readP_to_S readFinP

-- | reader for 'showFin'
readFinP :: forall n. PosC n => P.ReadP (Fin n)
readFinP = do
  P.skipSpaces
  (i, n) <- P.between (P.string "Fin(") (P.string ")") ((,) <$> pPosInt <* P.char ',' <*> pPosInt)
  either (const P.pfail) pure (mkFinC i n)

instance Show (Fin n) where
  show = showFin

-- | class for constraining "i" to positive numbers less than or equal to "n"
type FinC :: Nat -> Nat -> Constraint
class (PosC i, PosC n) => FinC i n where
  finC :: Fin n

instance (PosC n, i <=! n) => FinC i n where
  finC = Fin' (fromNP @i) (fromNP @n)

-- | class for constraining "i" to positive numbers less than or equal to "n" with a custom error message
type FinWithMessageC :: GL.ErrorMessage -> Nat -> Nat -> Constraint
class FinWithMessageC msg i n

instance LTEQT msg i n => FinWithMessageC msg i n

-- | type synonym for index 1
_F1 :: FinC 1 n => Fin n
_F1 = finC @1

-- | type synonym for index 2
_F2 :: FinC 2 n => Fin n
_F2 = finC @2

-- | type synonym for index 3
_F3 :: FinC 3 n => Fin n
_F3 = finC @3

-- | type synonym for index 4
_F4 :: FinC 4 n => Fin n
_F4 = finC @4

-- | type synonym for index 5
_F5 :: FinC 5 n => Fin n
_F5 = finC @5

-- | type synonym for index 6
_F6 :: FinC 6 n => Fin n
_F6 = finC @6

-- | type synonym for index 7
_F7 :: FinC 7 n => Fin n
_F7 = finC @7

-- | type synonym for index 8
_F8 :: FinC 8 n => Fin n
_F8 = finC @8

-- | type synonym for index 9
_F9 :: FinC 9 n => Fin n
_F9 = finC @9

-- | type synonym for index 10
_F10 :: FinC 10 n => Fin n
_F10 = finC @10

-- | type synonym for index 11
_F11 :: FinC 11 n => Fin n
_F11 = finC @11

-- | type synonym for index 12
_F12 :: FinC 12 n => Fin n
_F12 = finC @12

-- | type synonym for index 13
_F13 :: FinC 13 n => Fin n
_F13 = finC @13

-- | type synonym for index 14
_F14 :: FinC 14 n => Fin n
_F14 = finC @14

-- | type synonym for index 15
_F15 :: FinC 15 n => Fin n
_F15 = finC @15

-- | type synonym for index 16
_F16 :: FinC 16 n => Fin n
_F16 = finC @16

-- | type synonym for index 17
_F17 :: FinC 17 n => Fin n
_F17 = finC @17

-- | type synonym for index 18
_F18 :: FinC 18 n => Fin n
_F18 = finC @18

-- | type synonym for index 19
_F19 :: FinC 19 n => Fin n
_F19 = finC @19

-- | type synonym for index 20
_F20 :: FinC 20 n => Fin n
_F20 = finC @20
