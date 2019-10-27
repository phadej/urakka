{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Urakka.Ref (
    Urakka (..),
    UrakkaRef (..),
    urakka,
    ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.DeepSeq        (NFData, force)
import Control.Exception      (evaluate)
import Control.Selective      (Selective)
import Data.IORef             (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe       (unsafePerformIO)
import Type.Reflection        (TypeRep, Typeable, typeRep)

import Urakka.Free

-- | 'Urakka' is a /task/ in this build system.
newtype Urakka a = Urakka (Free UrakkaRef a)
  deriving newtype (Functor, Applicative,  Selective)

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

-- | TODO: we'll need a variant returning ref,
-- as then we can watch individual task progress
urakka
    :: (Typeable a, Typeable b, NFData b)
    => Urakka a
    -> (a -> IO b)
    -> IO (Urakka b)
urakka (Urakka x) k = do
    var <- newTVarIO $ Left $ \a -> do
        b <- k a
        evaluate (force b)

    c <- atomicModifyIORef' counterRef $ \n -> (succ n, n)
    let ref = UrakkaRef c typeRep typeRep var
    -- record ref
    return (Urakka (Unpure ref x))

instance HasId UrakkaRef where
    getId (UrakkaRef c _ _ _) = c
