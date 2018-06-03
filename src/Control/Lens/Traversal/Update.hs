{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
module Control.Lens.Traversal.Update where

import Control.Applicative
import Control.Lens
import Data.Monoid

-- | An 'Update' is a function acting conditionally
-- as an endomorphism.
newtype Update a = Update { toUpdate :: (a -> a, a -> Bool) }

-- apply is the morphism of monoids from Update's to Endomorphisms.
apply :: Update a -> a -> a
apply (Update (f, c)) = Endo (\x -> if c x then f x else x)
{-# INLINE apply #-}

-- (f, c) . (f', c') = (f . f', c . f' or c')
-- (f1, c1) . (f2, c2) . (f3, c3) = (f1 f2 f3, c1 f2 f3 or c2 f3 or c3)
compose :: Update a -> Update a -> Update a
compose (Update (f, c)) (Update (f', c')) = Update (f . f', cond)
   where cond x = c' x || c (f' x)
{-# INLINE compose #-}

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
overIf l (Update (f, c)) = Update (over l f, (getAny . getConst) . viewPred l (Const . Any . c))
{-# INLINE overIf #-}

viewPred :: Traversal' s a -> (a -> Const Any a) -> (s -> Const Any s)
viewPred t = t
{-# INLINE viewPred #-}
