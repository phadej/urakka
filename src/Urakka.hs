{-# LANGUAGE GADTs #-}
-- | Urakka: simple build system.
module Urakka (
    -- * Construction
    Urakka,
    urakka,
    urakka',
    urakkaSTM,
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
if_ :: Urakka a Bool -> Urakka () b -> Urakka () b -> Urakka a b
if_ (Urakka x) (Urakka y) (Urakka z) = Urakka $ ifFree x y z
