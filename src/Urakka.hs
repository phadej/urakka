{-# LANGUAGE GADTs #-}
-- | Urakka: simple build system.
module Urakka (
    -- * Construction
    Urakka,
    urakka,
    if_,
    -- * Running
    runSerial,
    runConcurrent,
    ) where

import Urakka.Free
import Urakka.Ref
import Urakka.Serial
import Urakka.Concurrent

-- | Conditional executuion.
if_ :: Urakka Bool -> Urakka a -> Urakka a -> Urakka a
if_ (Urakka x) (Urakka y) (Urakka z) = Urakka $ ifFree x y z
