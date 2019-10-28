{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Urakka.Free where

import Control.Selective  (Selective (..))

import Control.Category (Category (..))
import Control.Arrow (Arrow (..), ArrowChoice (..), returnA, (>>>))

import Prelude hiding (id, (.))

import qualified Data.IntSet as IS

class HasId p where
    getId :: p a b -> Int

data Free p a b where
    Pure :: (a -> b)
         -> Free p a b

    Comp :: (a -> b)
         -> p b c
         -> Free p c d
         -> Free p a d

    Mult :: (x -> (a,c))
         -> Free p a b
         -> Free p c d
         -> Free p (b, d) y
         -> Free p x y

    Choi :: (x -> Either a c)
         -> Free p a b
         -> Free p c d
         -> Free p (Either b d) y
         -> Free p x y

instance Category (Free p) where
    id = Pure id
    
    f . Pure g       = lmap g f
    f . Comp h p g   = Comp h p (f . g)
    f . Mult h x y g = Mult h x y (f . g)
    f . Choi h x y g = Choi h x y (f . g)

lmap :: (a -> b) -> Free p b c -> Free p a c
lmap f (Pure g)       = Pure (g . f)
lmap f (Comp h p g)   = Comp (h . f) p g
lmap f (Mult h x y g) = Mult (h . f) x y g
lmap f (Choi h x y g) = Choi (h . f) x y g

toA :: Arrow a => a () (b -> c) -> a b c
toA f = arr (\x -> ((), x)) >>> first f >>> arr (uncurry ($))

instance Arrow (Free p) where
    arr = Pure
    f *** g = Mult id f g id

instance ArrowChoice (Free p) where
    f +++ g = Choi id f g id

instance Functor (Free p a) where
    fmap f x = x >>> arr f

instance Applicative (Free p a) where
    pure = arr . const
    f <*> x = f &&& x >>> arr (uncurry ($))

instance a ~ () => Selective (Free p a) where
    select x y = x >>> toA y ||| returnA

data Necessary p where
    Necessary :: p a b -> a -> Necessary p

necessary :: Free p a b -> a -> Either (Necessary p) b
necessary (Pure f)       x = Right (f x)
necessary (Comp f p _)   x = Left (Necessary p (f x))
necessary (Mult f a b g) x = do
    let (y, z) = f x
    a' <- necessary a y
    b' <- necessary b z
    necessary g (a', b')
necessary (Choi f a b g) x = case f x of
    Left y -> do
        a' <- necessary a y
        necessary g (Left a')
    Right z -> do
        b' <- necessary b z
        necessary g (Right b')

flippedSimplify :: Free p x y -> (forall a b. p a b -> Maybe b) -> Free p x y
flippedSimplify x f = simplify f x

simplify :: (forall a b. p a b -> Maybe b) -> Free p x y -> Free p x y
simplify _ x@Pure  {} = x
simplify f (Comp h p g) = case f p of
    Nothing -> Comp h p (simplify f g)
    Just b  -> arr (const b) >>> simplify f g
simplify f (Mult h a b g) = Mult h (simplify f a) (simplify f b) (simplify f g)
simplify f (Choi h a b g) = Choi h (simplify f a) (simplify f b) (simplify f g)

ifFree :: Free p a Bool -> Free p () b -> Free p () b -> Free p a b
ifFree x y z = proc a -> do
    b <- x -< a
    if b then y -< () else z -< () 

overEstimateFree :: HasId p => Free p a b -> Int
overEstimateFree = IS.size . go where
    go :: HasId p => Free p a b -> IS.IntSet
    go (Pure _)       = IS.empty
    go (Comp _ p g)   = IS.insert (getId p) (go g)
    go (Mult _ a b g) = IS.union (go g) (IS.union (go a) (go b))
    go (Choi _ a b g) = IS.union (go g) (IS.union (go a) (go b))

underEstimateFree :: HasId p => Free p a b -> Int
underEstimateFree = IS.size . go where
    go :: HasId p => Free p a b -> IS.IntSet
    go (Pure _)       = IS.empty
    go (Comp _ p g)   = IS.insert (getId p) (go g)
    go (Mult _ a b g) = IS.union (go g) (IS.union (go a) (go b))
    go (Choi _ a b g) = IS.union (go g) (IS.intersection (go a) (go b))

debug :: forall p x y. (forall a b. p a b -> ShowS) -> Free p x y -> ShowS
debug sp = go 0 where
    go :: Int -> Free p u v -> ShowS
    go d (Pure _)       = showParen (d > 10) $ showString "(arr _)"
    go d (Comp _ p g)   = showParen (d > 1) $ sp p . showString " >>> " . go 1 g
    go d (Mult _ a b g) = showParen (d > 1)
        $ showParen (d > 3) $ go 3 a . showString " *** " . go 2 b
        . showString " >>> "
        . go 1 g
    go d (Choi _ a b g) = showParen (d > 1)
        $ showParen (d > 2) $ go 2 a . showString " +++ " . go 1 b
        . showString " >>> "
        . go 1 g
