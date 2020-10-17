-- | Concurrent executor.
module Urakka.Concurrent (
    -- * Run concurrently
    runConcurrent,
    -- * Advanced use
    runConcurrent',
    ConcSt,
    urakkaDone,
    urakkaQueued,
    urakkaOverEstimate,
    ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.STM
       (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry,
       writeTVar)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS

import Urakka.Free
import Urakka.Ref

-- | Concurrent state
data ConcSt = ConcSt
    { psQueued :: TVar IS.IntSet
    , psAll    :: TVar (IM.IntMap Int)
    , psDone   :: TVar Int
    }

-- | Amount of already done tasks.
urakkaDone :: ConcSt -> STM Int
urakkaDone = readTVar . psDone

-- | Over-estimate of total tasks, updated during running.
urakkaOverEstimate :: ConcSt -> STM Int
urakkaOverEstimate = fmap IM.size . readTVar . psAll

-- | Amount of total enqueued (including done) tasks.
--
-- Grows, but not over 'overEstimate' count.
urakkaQueued :: ConcSt -> STM Int
urakkaQueued = fmap IS.size . readTVar . psQueued

newConcSt :: IM.IntMap Int -> IO ConcSt
newConcSt a' = do
    q <- newTVarIO IS.empty
    a <- newTVarIO a'
    d <- newTVarIO 0
    return $ ConcSt q a d

-- | Run tasks concurrently.
runConcurrent :: a -> Urakka a b -> IO b
runConcurrent a u = do
    (x, _) <- runConcurrent' (\_ -> return ()) a u
    wait x

-- | Run tasks concurrently, return 'Async' action,
-- and a 'ConcSt' so progress can be checked.
runConcurrent'
    :: (ConcSt -> IO ())    -- ^ action to run after each completed job
    -> a                    -- ^ initial value
    -> Urakka a b           -- ^ urakka to do
    -> IO (Async b, ConcSt)
runConcurrent' onDone a0 (Urakka u) = do
    st <- newConcSt (overEstimateFree' u)
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
            atomically $ do
                est <- readTVar (psAll st)
                let est' = IM.differenceWith sub est (overEstimateFree' z)
                writeTVar (psAll st) est'
            go st (Left y2) g
        Right z' -> do
            z2 <- go st z' z
            atomically $ do
                est <- readTVar (psAll st)
                let est' = IM.differenceWith sub est (overEstimateFree' y)
                writeTVar (psAll st) est'
            go st (Right z2) g
    go st@(ConcSt queued _all done) a (Comp h (UrakkaRef c _trA _trB ref) g) = do
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
            else do
                modifyTVar' queued (IS.insert c)
                readTVar ref

        -- if there's result, return it; otherwise perform an action
        b <- case res of
            Right b -> return b
            Left k -> do
                b <- k (h a)
                atomically $ do
                    writeTVar ref (Right b)
                    modifyTVar' done succ
                return b

        -- do something
        onDone st

        -- continue with a next pipe
        go st b g

    sub :: Int -> Int -> Maybe Int
    sub a b | b >= a    = Nothing
            | otherwise = Just (b - a)
