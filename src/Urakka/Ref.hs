{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Urakka.Ref (
    Urakka (..),
    UrakkaRef (..),
    urakka,
    urakka',
    urakkaSTM,
    ) where

import Control.Arrow           (Arrow ((&&&)), ArrowChoice, returnA, (>>>))
import Control.Category        (Category)
import Control.Concurrent.STM  (STM, TVar, newTVarIO, readTVar, retry)
import Control.DeepSeq         (NFData, force)
import Control.Exception       (evaluate)
import Control.Monad.IO.Class  (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Selective       (Selective (..))
import Data.IORef              (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe        (unsafePerformIO)
import Type.Reflection         (TypeRep, Typeable, typeRep)

import Urakka.Free


-- | 'Urakka' is a /task/ in this build system.
newtype Urakka a b = Urakka (Free UrakkaRef a b)
  deriving (Functor, Applicative, Category, Arrow, ArrowChoice)

instance Selective (Urakka a) where
    select = select_

-- | Implementation of 'select'
select_ :: Urakka a (Either b c) -> Urakka a (b -> c) -> Urakka a c
select_ (Urakka a) (Urakka b) = Urakka (selectFree a b)

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

-- | Create a 'Urakka' task. 'Functor' way, think @\<**\>@.
urakka
    :: (Typeable b, Typeable c, NFData c, MonadUnliftIO m)
    => Urakka a b
    -> (b -> m c)
    -> m (Urakka a c)
urakka g k = do
    u <- urakka' k
    return (g >>> u)

-- | Create an 'Urakka' task. 'Arrow' way.
--
-- This function should be used carefully.
-- The underlying @a -> m b@ will be evaluated only once
-- folding into '\_ -> return result', and then shared across
-- whole computation.
--
-- /TODO:/ make variant which differentiates inputs.
--
urakka'
    :: (Typeable a, Typeable b, NFData b, MonadUnliftIO m)
    => (a -> m b)
    -> m (Urakka a b)
urakka' = fmap snd . urakkaSTM

-- | 'urakka'' which also returns a 'STM' action, which can be used to track progress.
--
-- See note on 'urakka''.
urakkaSTM
    :: (Typeable a, Typeable b, NFData b, MonadUnliftIO m)
    => (a -> m b)
    -> m (STM b, Urakka a b)
urakkaSTM k = withRunInIO $ \runInIO -> do
    -- a TVar holding either an IO action, or a result
    var <- liftIO $ newTVarIO $ Left $ \a -> do
        b <- runInIO (k a)
        evaluate (force b)

    -- a STM action to check whether task is completed
    let stm = do
            res <- readTVar var
            case res of
                Left _  -> retry
                Right x -> return x

    c <- atomicModifyIORef' counterRef $ \n -> (succ n, n)
    let ref = UrakkaRef c typeRep typeRep var
    -- record ref
    return (stm, Urakka (Comp id ref returnA))

instance HasId UrakkaRef where
    getId (UrakkaRef c _ _ _) = c

instance Semigroup b => Semigroup (Urakka a b) where
    x <> y = (x &&& y) >>> Urakka (Pure (uncurry (<>)))

instance Monoid b => Monoid (Urakka a b) where
    mempty = Urakka (Pure (const mempty))
