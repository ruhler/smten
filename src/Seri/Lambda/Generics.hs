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
    tm_NType :: a -> NType -> m NType
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
    tm_NType _ = return
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
    transformM f (NumT n) = do
        n' <- transformM f n
        tm_Type f (NumT n')
    transformM f t = tm_Type f t

instance (Monad m) => TransformableM NType m where
    transformM f (AppNT op a b) = do
        a' <- transformM f a
        b' <- transformM f b
        tm_NType f (AppNT op a' b')
    transformM f t = tm_NType f t

instance (Monad m) => TransformableM Exp m where
    transformM f e@(LitE {}) = tm_Exp f e
    transformM f (ConE s) = do
        s' <- transformM f s
        tm_Exp f (ConE s')
    transformM f (VarE s) = do
        s' <- transformM f s
        tm_Exp f (VarE s')
    transformM f (LaceE ms) = do
        ms' <- transformM f ms
        tm_Exp f (LaceE ms')
    transformM f (AppE a b) = do
        a' <- transformM f a
        b' <- transformM f b
        tm_Exp f (AppE a' b')

instance (Monad m) => TransformableM Pat m where
    transformM f (ConP t n ps) = do
        t' <- transformM f t
        ps' <- transformM f ps
        tm_Pat f (ConP t' n ps')
    transformM f (VarP s) = do
        s' <- transformM f s
        tm_Pat f (VarP s')
    transformM f p@(LitP {}) = tm_Pat f p
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

instance (Monad m, TransformableM a m, TransformableM b m)
    => TransformableM (a, b) m where
    transformM f (a, b) = do
        a' <- transformM f a
        b' <- transformM f b
        return (a', b')

instance (Monad m) => TransformableM Char m where
    transformM f c = return c
        

-- | Same as TransformerM, only for pure transformations instead of monadic.
class Transformer a where
    -- TODO: add more parts of the IR as needed.
    t_NType :: a -> NType -> NType
    t_Type :: a -> Type -> Type
    t_Exp :: a -> Exp -> Exp
    t_Pat :: a -> Pat -> Pat
    t_Sig :: a -> Sig -> Sig
    t_Match :: a -> Match -> Match
    t_Class :: a -> Class -> Class
    t_Dec :: a -> Dec -> Dec
    t_TopSig :: a -> TopSig -> TopSig
    t_Con :: a -> Con -> Con
    t_Method :: a -> Method -> Method

    -- Everything defaults to no transformation.
    t_NType _ = id
    t_Type _ = id
    t_Exp _ = id
    t_Pat _ = id
    t_Sig _ = id
    t_Match _ = id
    t_Class _ = id
    t_Dec _ = id
    t_TopSig _ = id
    t_Con _ = id
    t_Method _ = id

class Transformable a where
    transform :: (Transformer f) => f -> a -> a

instance  Transformable Type where
    transform f (AppT a b) = t_Type f $ AppT (transform f a) (transform f b)
    transform f (NumT n) = t_Type f $ NumT (transform f n)
    transform f t = t_Type f t

instance Transformable NType where
    transform f (AppNT op a b)
        = t_NType f $ AppNT op (transform f a) (transform f b)
    transform f t = t_NType f t

instance Transformable Exp where
    transform f e@(LitE {}) = t_Exp f e
    transform f (ConE s) = t_Exp f $ ConE (transform f s)
    transform f (VarE s) = t_Exp f $ VarE (transform f s)
    transform f (AppE a b) = t_Exp f $ AppE (transform f a) (transform f b)
    transform f (LaceE ms) = t_Exp f $ LaceE (transform f ms)

instance Transformable Pat where
    transform f (ConP t n ps) = t_Pat f $ ConP (transform f t) n (transform f ps)
    transform f (VarP s) = t_Pat f $ VarP (transform f s)
    transform f p@(LitP {}) = t_Pat f p
    transform f (WildP t) = t_Pat f $ WildP (transform f t)

instance Transformable Sig where
    transform f (Sig n t) = t_Sig f $ Sig n (transform f t)

instance (Transformable a) => Transformable [a] where
    transform f = map (transform f)

instance Transformable Match where
    transform f (Match p b) = t_Match f $ Match (transform f p) (transform f b)

instance Transformable Class where
    transform f (Class n ts) = t_Class f $ Class n (transform f ts)

instance Transformable Dec where
    transform f (ValD ts e) = t_Dec f $ ValD (transform f ts) (transform f e)
    transform f (DataD n vars cons) = t_Dec f $ DataD n vars (transform f cons)
    transform f (ClassD n vars sigs) = t_Dec f $ ClassD n vars (transform f sigs)
    transform f (InstD ctx cls ms) = t_Dec f $ InstD (transform f ctx) (transform f cls) (transform f ms)
    transform f (PrimD ts) = t_Dec f $ PrimD (transform f ts)

instance Transformable TopSig where
    transform f (TopSig n ctx t) = t_TopSig f $ TopSig n (transform f ctx) (transform f t)

instance Transformable Con where
    transform f (Con n ts) = t_Con f $ Con n (transform f ts)

instance Transformable Method where
    transform f (Method n e) = t_Method f $ Method n (transform f e)

instance (Transformable a, Transformable b) => Transformable (a, b) where
    transform f (a, b) = (transform f a, transform f b)

instance Transformable Char where
    transform f c = c

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

