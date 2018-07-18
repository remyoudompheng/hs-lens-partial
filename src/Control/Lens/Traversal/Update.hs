{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
module Control.Lens.Traversal.Update where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Semigroup
import qualified Data.Vector as V

-- | An 'Update' is a function acting conditionally
-- as an endomorphism.
newtype Update a = Update { getUpdate :: (a -> a, a -> Bool) }

-- apply is the morphism of monoids from Update's to Endomorphisms.
apply :: Update a -> a -> a
apply (Update (f, c)) x = if c x then f x else x
{-# INLINABLE apply #-}

-- (f, c) . (f', c') = (f . (f' if c'), c . f' or c')
-- (f1, c1) . (f2, c2) . (f3, c3) = (f1 (f2 if c2) (f3 if c3), c1 f2 f3 or c2 f3 or c3)
--
-- We have the law:
-- apply (compose u v) = apply u . apply v
compose :: Update a -> Update a -> Update a
compose (Update (f, c)) y@(Update (f', c')) = Update (f . apply y, cond)
   where cond x = c' x || c (f' x)
{-# INLINABLE compose #-}

instance Semigroup (Update a) where
   (<>) = compose

instance Monoid (Update a) where
   mempty = Update (id, const True)
   mappend = compose

-- | Updates can be used with Traversals
--
-- * we can lift x :: a -> a to s -> s
--   if l supports functor f = Identity
--   (okay for Traversal)
-- * we can lift c :: a -> Bool to s -> Bool
--   if l supports functor f = Const Bool
--   (okay for Traversal, using monoid law || on Bool)
-- 
-- We have the law:
-- compose (overIf l f) (overIf l g) = overIf l (f.g)
--
-- On the first component:
--     over l (f . f') = over l f . over l f'
-- On the second component:
--     any (c f' or c') = (any c) f' or (any c)
--
-- The lifted predicate means that there is an update
-- if any of the elements of the traversal is updated.
overIf :: Traversal' s a -> Update a -> Update s
overIf l up@(Update (f, c)) = Update (over l (apply up), (getAny . getConst) . viewPred l (Const . Any . c))
{-# INLINE overIf #-}

-- overVector is a hand-written specialization of overIf
overVector :: Update a -> Update (V.Vector a)
overVector up@(Update (f, c)) = Update (V.map (apply up), V.any c)
{-# INLINE overVector #-}
-- We would like the following rules
--     forall (l :: Traversal' s a) (l' :: Traversal' a b). overIf (l . l') = (overIf l) . (overIf l')
--     overIf traverse = overVector

viewPred :: Traversal' s a -> (a -> Const Any a) -> (s -> Const Any s)
viewPred t = t
{-# INLINABLE viewPred #-}
