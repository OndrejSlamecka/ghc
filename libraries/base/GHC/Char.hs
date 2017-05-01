{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Char
    ( -- * Utilities
      chr

      -- * Monomorphic equality operators
      -- | See GHC.Classes#matching_overloaded_methods_in_rules
    , eqChar, neChar
    ) where

import GHC.Base
import GHC.Show

-- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
-- The input integer has to be in the range [0..0x10FFFF]
-- as defined in <https://tools.ietf.org/html/rfc3629 RFC3629>.
-- Note: 0x10FFFF hexadecimal equals 11114111 decimal.
-- TODO: When ord's type is restricted, the type of chr's output should
-- be restricted so that chr . ord passes LH check.
{-@ chr :: {v:Int | v >= 0 && v <= 1114111 } -> Char @-}
chr :: Int -> Char
chr i@(I# i#)
 | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
 | otherwise
    = die {-errorWithoutStackTrace-} ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")


-- Example (placed here just to showcase the use, wouldn't be present in
-- real code), run `liquid Char.hs` to observe error on the line with
-- 'problem':
at_sign = chr 64 -- is ok, equals '@'
problem = chr 1114112 -- fail


-- I had to create this to replace errorWithoutStackTrace which liquid
-- refused to recognize (even though GHC.Err is imported in GHC.Base).
--
-- The reason might be that GHC.Err uses TypeInType which liquid does
-- not support.
--
-- But the use of `errorWithoutStackTrace` has to be allowed, as the
-- implementation of chr cannot be changed (to stay compatible for
-- programmers not using liquid).
--
-- TODO: research how to make errorWithoutStackTrace work with liquid.
-- (And don't forget to add unsatisfiable predicate to its type as below.)
{-@ die :: String -> {v:_ | false} @-}
die = error


-- I had to add the following:
-- With this GHC complains about ambiguous occurence on line 10
-- but without this liquid complains with "GHC Error: Not in scope
-- 'eqChar' Perhaps you meant 'eqChar#'"
--
-- The reason might be that ghc-prim/GHC.Classes uses UndecidableSuperClasses
-- which liquid does not support.
--
-- TODO: find out how to make this file work without the following.
eqChar = eqChar#
neChar = neChar#
