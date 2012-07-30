-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic IR traversal routines.
module Seri.Lambda.Generics (
    TransformerM(..), transformM, TransformableM(),
    Transformer(..), transform, Transformable(),
    Querier(..), query, Queriable(),
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid

import Seri.Lambda.IR

-- | A TransformerM specifies monadic transformations for parts of the seri
-- IR. The type 'a' is a dummy type to select the right transformation
-- functions. It's value is discarded unused.
class (Monad m) => TransformerM a m where
    -- TODO: add more parts of the IR as needed.
    tm_Type :: a -> Type -> m Type
    tm_Exp :: a -> Exp -> m Exp
    tm_Pat :: a -> Pat -> m Pat
    tm_Sig :: a -> Sig -> m Sig
    tm_Match :: a -> Match -> m Match
    tm_Class :: a -> Class -> m Class
    tm_Dec :: a -> Dec -> m Dec
    tm_TopSig :: a -> TopSig -> m TopSig
    tm_Con :: a -> Con -> m Con
    tm_Method :: a -> Method -> m Method

    -- Everything defaults to no transformation.
    tm_Type _ = return
    tm_Exp _ = return
    tm_Pat _ = return
    tm_Sig _ = return
    tm_Match _ = return
    tm_Class _ = return
    tm_Dec _ = return
    tm_TopSig _ = return
    tm_Con _ = return
    tm_Method _ = return

class (Monad m) => TransformableM a m where
    -- | Bottom up recursive monadic transformation traversal of the given
    -- thing.
    transformM :: (TransformerM f m) => f -> a -> m a


instance (Monad m) => TransformableM Type m where
    transformM f (AppT a b) = do
        a' <- transformM f a
        b' <- transformM f b
        tm_Type f (AppT a' b')
    transformM f t = tm_Type f t

instance (Monad m) => TransformableM Exp m where
    transformM f e@(LitE {}) = tm_Exp f e
    transformM f (CaseE e ms) = do
        e' <- transformM f e
        ms' <- transformM f ms
        tm_Exp f (CaseE e' ms')
    transformM f (AppE a b) = do
        a' <- transformM f a
        b' <- transformM f b
        tm_Exp f (AppE a' b')
    transformM f (LamE s b) = do
        s' <- transformM f s
        b' <- transformM f b
        tm_Exp f (LamE s' b')
    transformM f (ConE s) = do
        s' <- transformM f s
        tm_Exp f (ConE s')
    transformM f (VarE s) = do
        s' <- transformM f s
        tm_Exp f (VarE s')

instance (Monad m) => TransformableM Pat m where
    transformM f (ConP t n ps) = do
        t' <- transformM f t
        ps' <- transformM f ps
        tm_Pat f (ConP t' n ps')
    transformM f (VarP s) = do
        s' <- transformM f s
        tm_Pat f (VarP s')
    transformM f p@(IntegerP {}) = tm_Pat f p
    transformM f (WildP t) = do
        t' <- transformM f t
        tm_Pat f (WildP t')

instance (Monad m) => TransformableM Sig m where
    transformM f (Sig n t) = do
        t' <- transformM f t
        tm_Sig f (Sig n t')

instance (Monad m, TransformableM a m) => TransformableM [a] m where
    transformM f = mapM (transformM f)

instance (Monad m) => TransformableM Match m where
    transformM f (Match p b) = do
        p' <- transformM f p
        b' <- transformM f b
        tm_Match f (Match p' b')

instance (Monad m) => TransformableM Class m where
    transformM f (Class n ts) = do
        ts' <- transformM f ts
        tm_Class f (Class n ts')

instance (Monad m) => TransformableM Dec m where
    transformM f (ValD ts e) = do
        ts' <- transformM f ts
        e' <- transformM f e
        tm_Dec f (ValD ts' e')
    transformM f (DataD n vars cons) = do
        cons' <- transformM f cons
        tm_Dec f (DataD n vars cons')
    transformM f (ClassD n vars sigs) = do
        sigs' <- transformM f sigs
        tm_Dec f (ClassD n vars sigs')
    transformM f (InstD ctx cls ms) = do
        ctx' <- transformM f ctx
        cls' <- transformM f cls
        ms' <- transformM f ms
        tm_Dec f (InstD ctx' cls' ms')
    transformM f (PrimD ts) = do
        ts' <- transformM f ts
        tm_Dec f (PrimD ts')

instance (Monad m) => TransformableM TopSig m where
    transformM f (TopSig n ctx t) = do
        ctx' <- transformM f ctx
        t' <- transformM f t
        tm_TopSig f (TopSig n ctx' t')

instance (Monad m) => TransformableM Con m where
    transformM f (Con n ts) = do
        ts' <- transformM f ts
        tm_Con f (Con n ts')

instance (Monad m) => TransformableM Method m where
    transformM f (Method n e) = do
        e' <- transformM f e
        tm_Method f (Method n e')
        

-- | Same as TransformerM, only for pure transformations instead of monadic.
class Transformer a where
    -- TODO: add more parts of the IR as needed.
    t_Type :: a -> Type -> Type

    -- Default to no transformation.
    t_Type _ = id

data PureTransformer f = PureTransformer f

instance (Transformer f) => TransformerM (PureTransformer f) Identity where
    tm_Type (PureTransformer f) x = return (t_Type f x)

class Transformable a where
    transform :: (Transformer f) => f -> a -> a

instance (TransformableM a Identity) => Transformable a where
    transform f x = runIdentity (transformM (PureTransformer f) x)


-- | Generic queries of seri stuff.
class (Monoid m) => Querier a m where
    -- TODO: add more as needed.
    q_Type :: a -> Type -> m
    q_Exp :: a -> Exp -> m
    q_Class :: a -> Class -> m

    -- Default to mempty
    q_Type _ _ = mempty
    q_Exp _ _ = mempty
    q_Class _ _ = mempty

data QueryTransformer q = QueryTransformer q

instance (Querier q m) => TransformerM (QueryTransformer q) (Writer m) where
    tm_Type (QueryTransformer q) x = tell (q_Type q x) >> return x
    tm_Exp (QueryTransformer q) x = tell (q_Exp q x) >> return x
    tm_Class (QueryTransformer q) x = tell (q_Class q x) >> return x

class (Monoid m) => Queriable a m where
    query :: (Querier q m) => q -> a -> m

instance (Monoid m, TransformableM a (Writer m)) => Queriable a m where
    query q x = execWriter (transformM (QueryTransformer q) x)

