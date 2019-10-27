module Main where

import Control.Applicative (liftA2)
import Control.Concurrent  (threadDelay)
import Data.Bits           (xor)
import System.Clock        (Clock (Monotonic), getTime, toNanoSecs)

import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?), assertBool)

import Urakka

main :: IO ()
main = defaultMain $ testGroup "example"
    [ testCase "serial" $ do
        u <- example
        (x, diff) <- clocked $ runSerial u

        debug $ show diff

        x @=? 'x'
        assertBool "Takes over 15ms" $ diff > 15 * 1000000

    , testCase "concurrent" $ do
        u <- example
        (x, diff) <- clocked $ runConcurrent u

        debug $ show diff

        x @=? 'x'
        assertBool "Takes under 15ms" $ diff < 15 * 1000000       
    ]

clocked :: IO a -> IO (a, Integer)
clocked m = do
    t0 <- getTime Monotonic
    x <- m
    t1 <- getTime Monotonic
    return (x, toNanoSecs $ t1 - t0)

-------------------------------------------------------------------------------
-- debug
-------------------------------------------------------------------------------

debug :: String -> IO ()
debug _ = return ()
-- debug = putStrLn

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

example :: IO (Urakka Char)
example = do
    x <- urakka (pure ()) $ \_ -> do
        debug "x"
        return 'x'

    y <- urakka (pure ()) $ \_ -> do
        debug "y"
        return 'y'

    coin1 <- urakka (pure ()) $ \_ -> do
        debug "coin1"
        threadDelay 10000
        return True

    coin2 <- urakka (pure ()) $ \_ -> do
        debug "coin2"
        threadDelay 10000
        return False

    let dep = if_ (liftA2 xor coin1 coin2) x y

    out1 <- urakka dep $ \z -> do
        debug $ "1. either one " ++ show z
        return z

    out2 <- urakka dep $ \z -> do
        debug $ "2. either one " ++ show z
        return z

    urakka (out1 *> out2) return
