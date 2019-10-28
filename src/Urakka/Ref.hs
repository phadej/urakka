{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Urakka.Ref (
    Urakka (..),
    UrakkaRef (..),
    urakka,
    urakka',
    ) where

import Control.Arrow           (Arrow, ArrowChoice, returnA, (>>>))
import Control.Category        (Category)
import Control.Concurrent.STM  (TVar, newTVarIO)
import Control.DeepSeq         (NFData, force)
import Control.Exception       (evaluate)
import Control.Monad.IO.Class  (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Selective       (Selective (..))
import Control.Selective       (Selective)
import Data.IORef              (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe        (unsafePerformIO)
import Type.Reflection         (TypeRep, Typeable, typeRep)

import Urakka.Free

-- | 'Urakka' is a /task/ in this build system.
newtype Urakka a b = Urakka (Free UrakkaRef a b)
  deriving newtype (Functor, Applicative, Category, Arrow, ArrowChoice)

instance a ~ () => Selective (Urakka a) where
    select (Urakka a) (Urakka b) = Urakka (select a b)

data UrakkaRef a b where
    UrakkaRef
        :: Int
        -> TypeRep a
        -> TypeRep b
        -> TVar (Either (a -> IO b) b)
        -> UrakkaRef a b

counterRef :: IORef Int
counterRef = unsafePerformIO (newIORef 0)
{-# NOINLINE counterRef #-}

urakka
    :: (Typeable b, Typeable c, NFData c, MonadUnliftIO m)
    => Urakka a b
    -> (b -> m c)
    -> m (Urakka a c)
urakka g k = do
    u <- urakka' k
    return (g >>> u)

-- as then we can watch individual task progress
urakka'
    :: (Typeable a, Typeable b, NFData b, MonadUnliftIO m)
    => (a -> m b)
    -> m (Urakka a b)
urakka' k = withRunInIO $ \runInIO -> do
    var <- liftIO $ newTVarIO $ Left $ \a -> do
        b <- runInIO (k a)
        evaluate (force b)

    c <- atomicModifyIORef' counterRef $ \n -> (succ n, n)
    let ref = UrakkaRef c typeRep typeRep var
    -- record ref
    return (Urakka (Comp id ref returnA))

instance HasId UrakkaRef where
    getId (UrakkaRef c _ _ _) = c
