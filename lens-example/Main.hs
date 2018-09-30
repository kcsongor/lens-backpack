module Main where

import Lens

data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving Show

name :: Lens Person Person String String
name = lens _name (\p n -> p { _name = n })

age :: Lens Person Person Int Int
age = lens _age (\p n -> p { _age = n })

main = putStrLn (view name (Person "Csongor" 8))
