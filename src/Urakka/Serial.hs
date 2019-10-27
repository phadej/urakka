{-# LANGUAGE GADTs #-}
-- | Serial executor.
module Urakka.Serial where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad          (guard)
import Data.Type.Equality     ((:~:) (..), testEquality)
import Data.List.NonEmpty (NonEmpty (..))

import Urakka.Free
import Urakka.Ref

-- | Run 'Urakka' completely serially.
runSerial :: Urakka a -> IO a
runSerial u = do
    res <- runSerialStep u
    case res of
        Right x -> return x
        Left u' -> runSerial u'

-- | Run one step of urakka, return either simplified 'Urakka', or final value.
runSerialStep :: Urakka a -> IO (Either (Urakka a) a)
runSerialStep (Urakka u) = case necessary u of
    Success x -> return (Right x)
    Failure (Necessary (UrakkaRef c trA trB ref) a :| _) -> do
        res <- readTVarIO ref
        b <- case res of
            Right b -> return b
            Left k -> do
                b <- k a
                atomically $ writeTVar ref (Right b)
                return b

        let u' = simplify u $ \(UrakkaRef c' trA' trB' ref') -> do
                guard (c == c')
                Refl <- testEquality trA trA'
                Refl <- testEquality trB trB'
                guard (ref == ref')
                return b

        return (Left (Urakka u'))
