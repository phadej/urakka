{-# LANGUAGE GADTs #-}
-- | Serial executor.
module Urakka.Serial where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad          (guard)
import Data.Type.Equality     ((:~:) (..), testEquality)

import Urakka.Free
import Urakka.Ref

-- | Run 'Urakka' completely serially.
runSerial :: a -> Urakka a b -> IO b
runSerial a u = do
    res <- runSerialStep a u
    case res of
        Right x -> return x
        Left u' -> runSerial a u'

-- | Run one step of urakka, return either simplified 'Urakka', or final value.
runSerialStep :: a -> Urakka a b -> IO (Either (Urakka a b) b)
runSerialStep a0 (Urakka u) = case necessary u a0 of
    Right x -> return (Right x)
    Left (Necessary (UrakkaRef c trA trB ref) a) -> do
        res <- readTVarIO ref
        b <- case res of
            Right b -> return b
            Left k -> do
                b <- k a
                atomically $ writeTVar ref (Right b)
                return b

        let u' = flippedSimplify u $ \(UrakkaRef c' trA' trB' ref') -> do
                guard $ c == c'
                Refl <- testEquality trA trA'
                Refl <- testEquality trB trB'
                guard $ ref == ref'
                return b

        return (Left (Urakka u'))
