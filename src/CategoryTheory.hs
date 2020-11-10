{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CategoryTheory where

class Monoidal f where
    unit :: () -> f ()
    (<~>) :: f a -> f b -> f (a, b)

instance (Monoidal f, Functor f) => Applicative f where
    pure = flip (<$) $ unit ()
    (<*>) = fmap (uncurry ($)) ... (<~>)

instance Applicative f => Monoidal f where
    unit = const $ pure ()
    (<~>) = (<*>) . fmap (,)

(...) = (.).(.) --blackbird

