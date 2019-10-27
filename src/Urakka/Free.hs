{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Urakka.Free where

import Control.Selective  (Selective (..))
import Data.Kind          (Type)
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.IntSet as IS

data Necessary p where
    Necessary :: p a b -> a -> Necessary p

class HasId p where
    getId :: p a b -> Int

data Free (f :: Type -> Type -> Type) (a :: Type) where
    Pure   :: a
           -> Free p a
    Ap     :: Free p (a -> b)
           -> Free p a
           -> Free p b
    Branch :: Free p (Either a b)
           -> Free p (a -> c)
           -> Free p (b -> c)
           -> Free p c

    Unpure :: p a b
           -> Free p a
           -> Free p b

instance Functor (Free f) where
    fmap f (Pure x)       = Pure (f x)
    fmap f (Ap x y)       = Ap (fmap (f .) x) y
    fmap f (Branch x y z) = Branch x (fmap (f .) y) (fmap (f .) z)

    fmap f x@Unpure {} = Ap (Pure f) x

instance Applicative (Free f) where
    pure = Pure

    Pure f <*> x = fmap f x
    f <*> Pure x = fmap ($ x) f

    f <*> x = Ap f x

instance Selective (Free f) where
    select a b = Branch a b (Pure id)

data Validation e a
    = Failure e
    | Success a

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success x) = Success (f x)

instance Semigroup e => Applicative (Validation e) where
    pure = Success

    Failure e <*> Failure x = Failure (e <> x)
    Failure e <*> Success _ = Failure e
    Success _ <*> Failure e = Failure e
    Success f <*> Success x = Success (f x)

-- TODO: necessary1
necessary :: Free p a -> Validation (NonEmpty (Necessary p)) a
necessary (Pure x) = pure x
necessary (Ap x y) = necessary x <*> necessary y
necessary (Branch x y z) = case necessary x of
    Failure e -> Failure e
    Success x' -> case x' of
        Left y'  -> necessary $ fmap ($ y') y
        Right z' -> necessary $ fmap ($ z') z
necessary (Unpure f x) = case necessary x of
    Failure e  -> Failure e
    Success x' -> Failure (Necessary f x' :| [])

ifFree :: Free f Bool -> Free f a -> Free f a -> Free f a
ifFree x y z = Branch (fmap fromBool x) (fmap const z) (fmap const y)
  where
    fromBool True = Right ()
    fromBool False = Left ()

simplify :: Free p x -> (forall a b. p a b -> Maybe b) -> Free p x
simplify (Unpure p x) f = case f p of
    Just b  -> Pure b
    Nothing -> Unpure p (simplify x f)

simplify (Pure x) _       = Pure x
simplify (Ap x y ) f      = Ap (simplify x f) (simplify y f)
simplify (Branch x y z) f = Branch (simplify x f) (simplify y f) (simplify z f)

overEstimateFree :: HasId p => Free p a -> Int
overEstimateFree = IS.size . go where
    go :: HasId p => Free p a -> IS.IntSet
    go (Pure _)       = IS.empty
    go (Ap f x)       = IS.union (go f) (go x)
    go (Unpure f x)   = IS.insert (getId f) (go x)
    go (Branch x y z) = IS.union (go x) $ IS.union (go y) (go z)

underEstimateFree :: HasId p => Free p a -> Int
underEstimateFree = IS.size . go where
    go :: HasId p => Free p a -> IS.IntSet
    go (Pure _)       = IS.empty
    go (Ap f x)       = IS.union (go f) (go x)
    go (Unpure f x)   = IS.insert (getId f) (go x)
    go (Branch x y z) = IS.union (go x) $ IS.intersection (go y) (go z)
