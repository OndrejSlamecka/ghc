-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires extended type classes)
--
-- Declaration of the Monoid class, and instances for list and functions.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Data.Monoid (
 	Monoid(..)
  ) where

import Prelude

-- ---------------------------------------------------------------------------
-- | The monoid class.
-- A minimal complete definition must supply 'mempty' and 'mappend',
-- and these should satisfy the monoid laws.

class Monoid a where
	mempty  :: a
	-- ^ Identity of 'mappend'
	mappend :: a -> a -> a
	-- ^ An associative operation
	mconcat :: [a] -> a

	-- ^ Fold a list using the monoid.
	-- For most types, the default definition for 'mconcat' will be
	-- used, but the function is included in the class definition so
	-- that an optimized version can be provided for specific types.

	mconcat = foldr mappend mempty

-- Monoid instances.

instance Monoid [a] where
	mempty  = []
	mappend = (++)

instance Monoid (a -> a) where
	mempty  = id
	mappend = (.)

instance Monoid () where
	-- Should it be strict?
	mempty        = ()
	_ `mappend` _ = ()
	mconcat _     = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
	mempty = (mempty, mempty)
	(a1,b1) `mappend` (a2,b2) =
		(a1 `mappend` a2, b1 `mappend` b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
	mempty = (mempty, mempty, mempty)
	(a1,b1,c1) `mappend` (a2,b2,c2) =
		(a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
	mempty = (mempty, mempty, mempty, mempty)
	(a1,b1,c1,d1) `mappend` (a2,b2,c2,d2) =
		(a1 `mappend` a2, b1 `mappend` b2,
		 c1 `mappend` c2, d1 `mappend` d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
		Monoid (a,b,c,d,e) where
	mempty = (mempty, mempty, mempty, mempty, mempty)
	(a1,b1,c1,d1,e1) `mappend` (a2,b2,c2,d2,e2) =
		(a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
		 d1 `mappend` d2, e1 `mappend` e2)

-- lexicographical ordering
instance Monoid Ordering where
	mempty         = EQ
	LT `mappend` _ = LT
	EQ `mappend` y = y
	GT `mappend` _ = GT
