
module Smten.Parser (
    -- * The Smten Language
    -- $doc
    parse
    ) where

import Smten.Parser.Grammar

-- $doc
-- The Smten language is a subset of haskell. The following lists the
-- differences between the Smten language and haskell as defined in the 
-- Haskell 2010 Language Report.
--
-- [@Things to implement eventually, perhaps@]
--
-- - Explicit module exports.
--
-- - Import specifications.
--
-- - Contexts in data declarations.
--
-- - deriving of classes other than Eq, Free, Show.
--
-- - Newtype declarations.
--
-- - Empty class declarations.
--
-- - Multiple vars for a single type signature.
-- 
-- - fixity declarations.
--
-- - ! in constructor declarations.
--
-- - infix constructors other than (:).
--
-- - left and right sections.
--
-- - irrefutable patterns.
--
-- - float literals.
--
-- - recursive let expressions. (Currently let expressions
--   are sequential, not recursive).
--
-- [@Things meant to be different from haskell@]
--
-- - Extra semicolons are often allowed.
--
-- - Multi-param type classes are supported.
--
-- - variable signatures not accompanied by an implementation are allowed,
-- indicating a primitive variable.
--
-- - Empty data declarations are supported, indicating a primitive type.
--
-- - Numeric types are supported, introduced with the # symbol.
--
-- - deriving of Free is supported
--
-- - Pattern type signatures are allowed.
--
--
-- [@Other Notes@]
--
-- - record constructors define variables for an undefined version of the
-- constructor (for Foo {}), and for updating (for x { foo = bar }).
-- This is not part of the haskell spec, but is used for implementation
-- purposes so label construction and update really is just syntactic sugar.
-- You probably shouldn't rely on this behavior.

