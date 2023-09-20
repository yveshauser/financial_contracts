{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Models
 ( P(..)
 , runFutT
 )
where

import Contracts
import Control.Applicative (Alternative, liftA2)
import Control.Monad.Identity (Identity, runIdentity)
import Derivatives
import Data.Int
import qualified Data.Massiv.Array as M
import Models.Futhark (getContext, runFutIn, runFutTIn, Output(..))
import qualified Models.Futhark.Entries as E
import Models.Futhark.TypeClasses (fromFO)
import Models.Futhark.Types (F64_1d)
import Models.Futhark.Context (Context(..))
import Models.Futhark.Fut
import Valuation

type Arr' b = M.Array M.D M.Ix1 b
newtype P c m a = P (FutT c m (Arr' a))

instance Functor m => Functor (P c m) where
  fmap f (P x) = let y = mapFutT g x in P y
    where
      g = fmap (fmap f)

instance Applicative m => Applicative (P c m) where
  pure a = P $ pure $ M.makeArray M.Seq (M.Sz 1600) (const a)
  (<*>) = undefined

instance Applicative m => Process (P c m) where
  ifP pb p1 p2 = g <$> zipP (,) pb (zipP (,) p1 p2)
    where
      g (True, p) = fst p
      g (False, p) = snd p

  zipP g (P p1) (P p2) = P $ let g' = liftA2 (M.zipWith g) in map2FutT g' p1 p2
  obs = undefined

instance (Monad m, Process (P c m)) => Model (P c m) where
   exch = undefined
   disc a (p1,p2) = P $ fmap M.delay run
   snell = undefined
   absorb = undefined

run :: Monad m => FutT c m (M.Array M.S M.Ix1 Double)
run = E.crrModel 6.0 5.0 0.04 0.3 1.9 (1600 :: Int64) >>= fromFuthark
