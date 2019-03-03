-- the pragmas needed for doctest need to be in the source file
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | Smallest possible Lens module
module Control.Lens.PicoLens where

import Data.Functor.Const
import Data.Functor.Identity

{- | Simplified Van Laarhoven lens compatible with lens/microlens

"Simplified" here means the types do not change (it is also possible to define
lenses that change the types around, these have 4 type parameters.

A lens can be produced with fmap by lifting the setting part over the functor of the getting thing.
E.g for accessing a type, the lenses would be:

> _1 :: Lens' (a, b) a
> _1 f (a, b) = fmap (\a' -> (a', b)) (f a)
> _2 :: Lens' (a, b) b
> _2 f (a, b) = fmap (\b' -> (a, b')) (f b)

These are not exactly getters, though.  If you have a getter and setter, you can
use the lens' function:
-}
type Lens' s a
   = forall f. Functor f =>
                 (a -> f a) -> (s -> f s)

-- $setup
-- >>> _1' = lens' fst (\(a,b) a' -> (a',b))
-- >>> _2' = lens' snd (\(a,b) b' -> (a,b'))
-- >>> testTup = ("a", 2) :: (String, Int)

{- | View the focus of the lens

>>> view _1 ("a", 2)
"a"
>>> view _2 ("a", 2)
2
-}
view :: ((a1 -> Const a1 b1) -> t -> Const a2 b2) -> t -> a2
view lens x = getConst $ lens Const x

{- | Modify the focus of the lens

>>> over _1 (++ "x") ("a", 2)
("ax",2)
>>> over _2 (+ 1) ("a", 2)
("a",3)
-}
over :: ((a1 -> Identity a2) -> t -> Identity a3) -> (a1 -> a2) -> t -> a3
over lens fn x = runIdentity $ lens (Identity . fn) x


-- | Lens for the first part of a tuple.
_1 :: Lens' (a, b) a
_1 f (a, b) = fmap (\a' -> (a', b)) (f a)

-- | Lens for the second part of a tupe.
_2 :: Lens' (a, b) b
_2 f (a, b) = fmap (\b' -> (a, b')) (f b)

-- not using quickcheck, it would have to be a dependency of the library

{- | Make a Var Varhoven lens from a getter and a setter.

$setup
>>> testTup = ("a", 2) :: (String, Int)

>>> view _1 testTup == view _1' testTup
True
>>> over _2 (+ 1) testTup == over _2' (+ 1) testTup
True
-}
lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens' getter setter f s = fmap (setter s) (f ( getter s))
