{-# LANGUAGE
      RankNTypes
    , DataKinds
    , TypeFamilies
    , InstanceSigs
    , TypeOperators
    , ExplicitForAll
    , TypeApplications
    , FlexibleInstances
    , ScopedTypeVariables
    , AllowAmbiguousTypes
    , UndecidableInstances
    , MultiParamTypeClasses
    , FunctionalDependencies
#-}

module Control.Monad.Apply
  ( appM
  , appM1
  , appM2
  , appM3
  , appM4
  , appM5
  , appM'
  , appM1'
  , appM2'
  , appM3'
  , appM4'
  , appM5'
  )
where

import Data.Kind (Type)
import Control.Monad (join)
import Prelude hiding (curry, uncurry)
import Control.Applicative (liftA, liftA2)

type family HArgs (xs :: [Type]) :: Type where
  HArgs '[] = ()
  HArgs (x ': xs) = (x, HArgs xs)

class Curryable f args res | f args -> res where
  curry :: f -> args -> res
  uncurry :: (args -> res) -> f

instance Curryable a () a where
  curry a () = a
  uncurry f = f ()

instance Curryable f args res => Curryable (a -> f) (a, args) res where
  curry :: (a -> f) -> (a, args) -> res
  curry f (a, args) = curry (f a) args

  uncurry :: ((a, args) -> res) -> a -> f
  uncurry f a = uncurry (\args -> f (a, args))

class WrapArgs m a b | m a -> b where
  unwrap :: b -> m a

instance {-# INCOHERENT #-}
  ( Applicative m )
  => WrapArgs m (a, ()) (m a, ()) where
    unwrap :: (m a, ()) -> m (a, ())
    unwrap (ma, ()) = pure (\a -> (a, ())) <*> ma

instance {-# INCOHERENT #-}
  (Applicative m, WrapArgs m a1 a2)
  => WrapArgs m (b, a1) (m b, a2) where
    unwrap :: (m b, a2) -> m (b, a1)
    unwrap (mb, a) = liftA2 (,) mb (unwrap a)

appM'
  :: forall m args1' args1 args2 res f g
   . ( Applicative m
     , HArgs args1' ~ args1
     , Curryable f args1 res
     , Curryable g args2 (m res)
     , WrapArgs m args1 args2
     )
  => f -> g
appM' f1 = uncurry g
 where
  f2 :: m args1 -> m res
  f2 = liftA $ curry f1

  g :: args2 -> m res
  g args = f2 (unwrap args)

appM
  :: forall m args1' args1 args2 res f g
   . ( Monad m
     , HArgs args1' ~ args1
     , Curryable f args1 (m res)
     , Curryable g args2 (m res)
     , WrapArgs m args1 args2
     )
  => f -> g
appM f1 = uncurry g
 where
  f2 :: m args1 -> m (m res)
  f2 = liftA $ curry f1

  g :: args2 -> m res
  g args = join $ f2 (unwrap args)

appM1 :: forall m a1 b
   . (Monad m)
  => (a1 -> m b)
  -> m a1 -> m b
appM1 = appM @m @'[a1]

appM2 :: forall m a1 a2 b
   . (Monad m)
  => (a1 -> a2 -> m b)
  -> m a1 -> m a2 -> m b
appM2 = appM @m @'[a1, a2]

appM3 :: forall m a1 a2 a3 b
   . (Monad m)
  => (a1 -> a2 -> a3 -> m b)
  -> m a1 -> m a2 -> m a3 -> m b
appM3 = appM @m @'[a1, a2, a3]

appM4 :: forall m a1 a2 a3 a4 b
   . (Monad m)
  => (a1 -> a2 -> a3 -> a4 -> m b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m b
appM4 = appM @m @'[a1, a2, a3, a4]

appM5 :: forall m a1 a2 a3 a4 a5 b
   . (Monad m)
  => (a1 -> a2 -> a3 -> a4 -> a5 -> m b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m b
appM5 = appM @m @'[a1, a2, a3, a4, a5]

appM1' :: forall m a b
   . (Applicative m)
  => (a -> b)
  -> m a -> m b
appM1' = appM' @m @'[a]

appM2' :: forall m a1 a2 b
   . (Applicative m)
  => (a1 -> a2 -> b)
  -> m a1 -> m a2 -> m b
appM2' = appM' @m @'[a1, a2]

appM3' :: forall m a1 a2 a3 b
   . (Applicative m)
  => (a1 -> a2 -> a3 -> b)
  -> m a1 -> m a2 -> m a3 -> m b
appM3' = appM' @m @'[a1, a2, a3]

appM4' :: forall m a1 a2 a3 a4 b
   . (Applicative m)
  => (a1 -> a2 -> a3 -> a4 -> b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m b
appM4' = appM' @m @'[a1, a2, a3, a4]

appM5' :: forall m a1 a2 a3 a4 a5 b
   . (Applicative m)
  => (a1 -> a2 -> a3 -> a4 -> a5 -> b)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m b
appM5' = appM' @m @'[a1, a2, a3, a4, a5]
