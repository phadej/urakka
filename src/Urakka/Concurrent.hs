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
runConcurrent :: a -> Urakka a b -> IO b
runConcurrent a u = do
    (x, _) <- runConcurrent' a u
    wait x

-- | Run tasks concurrently, return 'Async' action,
-- and a 'ConcSt' so progress can be checked.
runConcurrent' :: a -> Urakka a b -> IO (Async b, ConcSt)
runConcurrent' a0 (Urakka u) = do
    st <- newConcSt
    x <- async (go st a0 u)
    return (x, st)
  where
    go :: ConcSt -> a -> Free UrakkaRef a b -> IO b
    go _st a (Pure x) = return (x a)
    go st a (Mult f y z g) = do
        let (y', z') = f a
        y2 <- async (go st y' y)
        z2 <- async (go st z' z)
        y3 <- wait y2
        z3 <- wait z2
        go st (y3, z3) g
    go st a (Choi f y z g) = case f a of
        Left y' -> do
            y2 <- go st y' y
            go st (Left y2) g
        Right z' -> do
            z2 <- go st z' z
            go st (Right z2) g
    go st@(ConcSt queued done) a (Comp h (UrakkaRef c _trA _trB ref) g) = do
        -- Check whether the 'UrakkaRef' is already queued,
        -- if so, block until it's completed
        res <- atomically $ do
            q <- readTVar queued
            if c `IS.member` q
            then do
                res <- readTVar ref
                case res of
                    Right b -> return (Right b)
                    Left _  -> retry
            else readTVar ref
        
        -- if there's result, return it; otherwise perform an action
        b <- case res of
            Right b -> return b
            Left k -> do
                b <- k (h a)
                atomically $ do
                    writeTVar ref (Right b)
                    modifyTVar' done succ
                return b

        -- continue with a next pipe
        go st b g
