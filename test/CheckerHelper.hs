{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CheckerHelper where

import qualified GHC.Generics as G
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.Tasty
import qualified Test.Tasty.QuickCheck as TQ

checkersToProps :: [TestBatch] -> [(String, Property)]
checkersToProps = concatMap (\(a, bs) -> map (\(x, y) -> (a ++ " " ++ x, y)) bs)

adj' :: Bool -> Int -> Int -> Int -> TestTree -> TestTree
adj' v sz n ratio =
  adjustOption (const $ TQ.QuickCheckMaxSize sz)
    . adjustOption (max $ TQ.QuickCheckTests n)
    . adjustOption (max $ TQ.QuickCheckMaxRatio ratio)
    . adjustOption (const (TQ.QuickCheckVerbose v))

newtype MA = MA Int
  deriving stock (G.Generic, Show, Eq, Ord)
  deriving newtype (CoArbitrary, EqProp, Arbitrary)
newtype MB = MB Int
  deriving stock (G.Generic, Show, Eq, Ord)
  deriving newtype (CoArbitrary, EqProp, Arbitrary)
newtype MC = MC Int
  deriving stock (G.Generic, Show, Eq, Ord)
  deriving newtype (CoArbitrary, EqProp, Arbitrary)
newtype MD = MD Int
  deriving stock (G.Generic, Show, Eq, Ord)
  deriving newtype (CoArbitrary, EqProp, Arbitrary)

instance Semigroup MA where
  MA i <> MA j = MA (i + j)
instance Monoid MA where
  mempty = MA 0
  mappend = (<>)

instance Semigroup MB where
  MB i <> MB j = MB (i + j)
instance Monoid MB where
  mempty = MB 0
  mappend = (<>)

instance Semigroup MC where
  MC i <> MC j = MC (i + j)
instance Monoid MC where
  mempty = MC 0
  mappend = (<>)

instance Semigroup MD where
  MD i <> MD j = MD (i + j)
instance Monoid MD where
  mempty = MD 0
  mappend = (<>)

instance Function MA
instance Function MB
instance Function MC
instance Function MD
