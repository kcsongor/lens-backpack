{-# LANGUAGE Rank2Types #-}
module Lens where

data Lens s t a b
  = Lens (s -> a) (s -> b -> t)

view :: Lens s s a a -> s -> a
view (Lens g _) = g

modify :: Lens s t a b -> (a -> b) -> s -> t
modify (Lens g p) f s = p s (f (g s))

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens = Lens

(%) :: Lens s t a b -> Lens a b c d -> Lens s t c d
(%) (Lens g1 s1) (Lens g2 s2) = Lens (g2 . g1) (\s d -> s1 s (s2 (g1 s) d))
