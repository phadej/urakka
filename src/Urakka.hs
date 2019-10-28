{-# LANGUAGE GADTs #-}
-- | Urakka: simple build system.
module Urakka (
    -- * Construction
    Urakka,
    urakka,
    urakka',
    urakkaSTM,
    if_,
    -- * Running serially
    runSerial,
    runSerialStep,
    -- * Running concurrently
    runConcurrent,
    runConcurrent',
    ConcSt, urakkaDone, urakkaQueued,
    -- * Estimation
    -- | Estimation takes the task sharing into account.
    underEstimate,
    overEstimate,
    -- * Debugging
    structure,
    valid,
    ) where

import Urakka.Free
import Urakka.Ref
import Urakka.Serial
import Urakka.Concurrent

-- | Conditional executuion.
if_ :: Urakka a Bool -> Urakka a b -> Urakka a b -> Urakka a b
if_ (Urakka x) (Urakka y) (Urakka z) = Urakka $ ifFree x y z

-- | Print the structure of 'Urakka'.
structure :: Urakka a b -> ShowS
structure (Urakka u) = structureFree (\(UrakkaRef c _ _ _) -> showChar '#' . shows c) u

-- | Check whether internal invariants hold for an 'Urakka'.
valid :: Urakka a b -> Bool
valid (Urakka u) = validFree u

-- | Under-estimate the task count.
underEstimate :: Urakka a b -> Int
underEstimate (Urakka u) = underEstimateFree u

-- | Over-estimate the task count.
overEstimate :: Urakka a b -> Int
overEstimate (Urakka u) = overEstimateFree u
