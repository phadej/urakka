-- | Concurrent executor.
module Urakka.Concurrent (
    -- * Run concurrently
    runConcurrent,
    -- * Advanced use
    runConcurrent',
    ConcSt,
    urakkaDone,
    urakkaQueued,
    ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.STM
       (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)

import qualified Data.IntSet as IS

import Urakka.Free
import Urakka.Ref

-- | Concurrent state
data ConcSt = ConcSt
    { psQueued :: TVar IS.IntSet
    , psDone   :: TVar Int
    }

-- | Amount of already done tasks.
urakkaDone :: ConcSt -> STM Int
urakkaDone = readTVar . psDone

-- | Amount of enqueued (including done) tasks.
urakkaQueued :: ConcSt -> STM Int
urakkaQueued = fmap IS.size . readTVar . psQueued

newConcSt :: IO ConcSt
newConcSt = do
    q <- newTVarIO IS.empty
    d <- newTVarIO 0
    return $ ConcSt q d

-- | Run tasks concurrently.
runConcurrent :: Urakka a -> IO a
runConcurrent u = do
    (x, _) <- runConcurrent' u
    wait x

-- | Run tasks concurrently, return 'Async' action,
-- and a 'ConcSt' so progress can be checked.
runConcurrent' :: Urakka a -> IO (Async a, ConcSt)
runConcurrent' (Urakka u) = do
    st <- newConcSt
    x <- async (go st u)
    return (x, st)
  where
    go :: ConcSt -> Free UrakkaRef a -> IO a
    go _st (Pure x) = return x
    go st (Ap f x) = do
        f' <- async (go st f)
        x' <- async (go st x)
        f'' <- wait f'
        x'' <- wait x'
        return (f'' x'')
    go st (Branch x y z) = do
        x' <- go st x
        case x' of
            Left y' -> go st (fmap ($ y') y)
            Right z' -> go st (fmap ($ z') z)
    go st@(ConcSt queued done) (Unpure (UrakkaRef c _trA _trB ref) x) = do
        res <- atomically $ do
            q <- readTVar queued
            if c `IS.member` q
            then do
                res <- readTVar ref
                case res of
                    Right b -> return (Right b)
                    Left _  -> retry
            else readTVar ref

        case res of
            Right b -> return b
            Left k -> do
                a <- go st x
                b <- k a
                atomically $ do
                    writeTVar ref (Right b)
                    modifyTVar' done succ
                return b
