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

-- | smten compiler and interpreter.
module Main where

import Data.Generics

import qualified System.Console.CmdArgs.Implicit as A

import Smten.Failable
import Smten.Ppr
import Smten.Loader
import Smten.Module
import Smten.Typing
import Smten

import Smten.HaskellF.Compile

data Run = Phases | HaskellF
    deriving (Show, Eq, Typeable, Data)

data Args = Args {
    run :: Run,
    include :: [FilePath],
    file :: FilePath,
    output :: FilePath,
    hsdir :: FilePath
} deriving (Show, Eq, Data, Typeable)

argspec :: Args
argspec = Args { 
    run = A.enum [Phases A.&= A.help "Dump output from intermediate phases of compilation",
                  HaskellF A.&= A.help "Compile a smten program to Haskell"]
       A.&= A.typ "RUN MODE",
    include = []
       A.&= A.help "Smten include path" 
       A.&= A.typDir,
    file = "Main.smtn"
       A.&= A.help "Input .smtn file"
       A.&= A.typFile,
    output = "-"
       A.&= A.help "Where to place output. Use '-' for stdout"
       A.&= A.typFile,
    hsdir = "."
       A.&= A.help "Where to place generated .hs files for haskellf."
       A.&= A.typFile
    } A.&=
    A.verbosity A.&=
    A.help "Compile/Run a smten program" A.&=
    A.summary "smten" 

main :: IO ()
main = do
    args <- A.cmdArgs argspec
    stdlib <- smtendir

    let includes = include args ++ [stdlib]
    case (run args) of
        Phases -> do
            let outfphs :: String -> String -> IO ()
                outfphs ext text = case (output args) of
                            "-" -> do
                                putStrLn $ "Phase: " ++ ext
                                putStrLn text
                            fout -> writeFile (fout ++ "." ++ ext) text
            putStrLn "Loading modules..."
            mods <- loadmods includes (file args)
            outfphs "ldd" (pretty mods)

            putStrLn "sderiving..."
            sderived <- attemptIO $ sderive mods
            outfphs "sdr" (pretty sderived)

            putStrLn "qualifying..."
            qualified <- attemptIO $ qualify sderived
            outfphs "qlf" (pretty qualified)

            putStrLn "kind inferring..."
            kinded <- attemptIO $ kindinfer qualified
            outfphs "knd" (pretty kinded)

            putStrLn "type inferring..."
            inferred <- attemptIO $ typeinfer kinded
            outfphs "typ" (pretty inferred)

            putStrLn "type checking..."
            attemptIO $ typecheck inferred

        HaskellF -> do
            mods <- loadtyped includes (file args)
            haskellf (hsdir args) mods

