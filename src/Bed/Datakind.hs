{-# LANGUAGE DataKinds #-}

-- "-ddump-deriv" flag of interest

module Bed.Datakind where

-- Problem posed on #haskell

-- I have a type "Agent" with a set of constructors like "Java", "Php", "Python"
-- and so on. I would like a function that only takes `[Agent]` where each agent
-- in that list is only either Java, Php or Python. Not any combination. How can
-- I model this at the type system? so its either [Java, Java, Java] or [Python,
-- Python] but never [Java, Python] or [Java, Php] or [Python, Php] or some
-- combination


data Agent = Python | Php | Java
--
-- data SolidAgent :: * -> Agent where -- = 'Python | 'Php | 'Java
--   Python' :: SolidAgent Python
--   Php' :: SolidAgent Php
--   Java' :: SolidAgent Java


data JavaAgent = JavaAgent
data PythonAgent = PythonAgent
data PhpAgent = PhpAgent

data Agent' a = Agent [a]

class IsAgent a where
  isAgent :: a -> Bool

foo :: (IsAgent a) => [a] -> Bool
foo (b:_) = isAgent b

-- foo' :: OneAgent ->
