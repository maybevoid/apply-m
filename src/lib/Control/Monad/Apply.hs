{-# LANGUAGE
      RankNTypes
    , TypeFamilies
    , InstanceSigs
    , ExplicitForAll
    , TypeApplications
    , FlexibleInstances
    , AllowAmbiguousTypes
    , ScopedTypeVariables
    , MultiParamTypeClasses
#-}

module Control.Monad.Apply
  ( applyM
  , applyM1
  , applyM2
  , applyM3
  , applyM4
  , applyM5
  , applyPureM1
  , applyPureM2
  , applyPureM3
  , applyPureM4
  , applyPureM5
  )
where

import Data.Kind (Type)
import Control.Monad (join)
import Control.Applicative (liftA)

class Joinable m f where
  joinM :: m f -> f

instance Monad m
  => Joinable m (m a) where
    joinM = join

instance
  (Monad m, Joinable m f)
  => Joinable m (m a -> f) where
    joinM :: m (m a -> f) -> m a -> f
    joinM mf ma = joinM $ mf <*> pure ma

class LiftArg
    (m :: Type -> Type)
    (f :: Type)
    (g :: Type)
  where
    applyM :: f -> g

instance (Applicative m) => LiftArg m a (m a) where
  applyM :: a -> m a
  applyM = pure

instance LiftArg m (m a) (m a) where
  applyM :: m a -> m a
  applyM = id

instance (Applicative m, LiftArg m f g, Joinable m g)
  => LiftArg m (a -> f) (m a -> g) where
    applyM :: (a -> f) -> m a -> g
    applyM f ma = joinM mg
     where
      f' :: m a -> m f
      f' = liftA f

      mg :: m g
      mg = applyM @m <$> f' ma

applyM1 :: forall m a1 b . (Monad m)
  => (a1 -> m b)
  -> m a1 -> m b
applyM1 = applyM @m

applyM2 :: forall m a1 a2 b . (Monad m)
  => (a1 -> a2 -> m b)
  -> m a1 -> m a2 -> m b
applyM2 = applyM @m

applyM3 :: forall m a1 a2 a3 b . (Monad m)
  => (a1 -> a2 -> a3 -> m b)
  -> m a1 -> m a2 -> m a3 -> m b
applyM3 = applyM @m

applyM4 :: forall m a1 a2 a3 a4 b . (Monad m)
  => (a1 -> a2 -> a3 -> a4 -> m b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m b
applyM4 = applyM @m

applyM5 :: forall m a1 a2 a3 a4 a5 b . (Monad m)
  => (a1 -> a2 -> a3 -> a4 -> a5 -> m b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m b
applyM5 = applyM @m

applyPureM1 :: forall m a b . (Monad m)
  => (a -> b)
  -> m a -> m b
applyPureM1 = applyM @m

applyPureM2 :: forall m a1 a2 b . (Monad m)
  => (a1 -> a2 -> b)
  -> m a1 -> m a2 -> m b
applyPureM2 = applyM @m

applyPureM3 :: forall m a1 a2 a3 b . (Monad m)
  => (a1 -> a2 -> a3 -> b)
  -> m a1 -> m a2 -> m a3 -> m b
applyPureM3 = applyM @m

applyPureM4 :: forall m a1 a2 a3 a4 b . (Monad m)
  => (a1 -> a2 -> a3 -> a4 -> b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m b
applyPureM4 = applyM @m

applyPureM5 :: forall m a1 a2 a3 a4 a5 b . (Monad m)
  => (a1 -> a2 -> a3 -> a4 -> a5 -> b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m b
applyPureM5 = applyM @m
