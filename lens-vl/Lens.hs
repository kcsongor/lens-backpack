{-# LANGUAGE Rank2Types #-}
module Lens where

import Control.Applicative
import Data.Functor.Identity

newtype Lens s t a b = Lens (forall f. Functor f => (a -> f b) -> (s -> f t))

view :: Lens s s a a -> s -> a
view (Lens l) s = getConst (l Const s)

modify :: Lens s t a b -> (a -> b) -> s -> t
modify (Lens l) f s = runIdentity (l (Identity . f) s)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens g p = Lens (\f s -> p s <$> f (g s))
