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

{-# LANGUAGE DeriveDataTypeable #-}

-- | Main seri executable.
module Main where

import Data.Generics

import System.Environment
import System.Exit
import qualified System.Console.CmdArgs.Implicit as A

import Seri.Name
import Seri.Sig
import Seri.Failable
import Seri.ExpH
import Seri.Elaborate
import Seri.Ppr
import Seri.Dec
import Seri.Loader
import Seri.Module

import qualified Seri.SMT.Run as Q
import qualified Seri.SMT.Query as Q

import qualified Seri.IO.Run as I
import Seri.Haskell

data Run = Io | Type | Desugar | Haskell
    deriving (Show, Eq, Typeable, Data)

data Args = Args {
    run :: Run,
    include :: [FilePath],
    main_is :: String,
    file :: FilePath
} deriving (Show, Eq, Data, Typeable)

argspec :: Args
argspec = Args { 
    run = A.enum [Io A.&= A.help "Run a seri program in the IO monad",
                  Type A.&= A.help "Type infer and check a seri program",
                  Desugar A.&= A.help "Desugar, but don't type a seri program",
                  Haskell A.&= A.help "Compile a seri program to Haskell"]
       A.&= A.typ "RUN MODE",
    include = []
       A.&= A.help "Seri include path" 
       A.&= A.typDir,
    main_is = "Main.main"
       A.&= A.help "Fully qualified top-level function to use",
    file = "Main.sri"
       A.&= A.help "Input .sri file"
       A.&= A.typFile
    } A.&=
    A.verbosity A.&=
    A.help "Compile/Run a seri program" A.&=
    A.summary "seri" 

main :: IO ()
main = do
    args <- A.cmdArgs argspec


    let nmain = name (main_is args)

    case (run args) of
        Io -> do 
            env <- loadenv (include args) (file args)
            tmain <- attemptIO $ lookupVarType env nmain
            let m = varEH (Sig (name (main_is args)) tmain)
            I.run (mkEnvH env) m
            return ()
        Desugar -> do
            mods <- load (include args) (file args)
            flat <- attemptIO $ flatten mods
            putStrLn . pretty $ flat
        Type -> do
            env <- loadenv (include args) (file args)
            putStrLn . pretty $ env
        Haskell -> do
            env <- loadenv (include args) (file args)
            putStrLn . show $ haskell (getDecls env) nmain

