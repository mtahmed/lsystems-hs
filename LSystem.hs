module LSystem where

import Data.Map     ( Map, (!), fromList, findWithDefault )

-- |Simple LSystem datatype
data LSystem a = LSystem { transitions :: Map a [a], state :: [a] }
  deriving Show

-- |Construct an LSystem from a list of transitions and a start state
lsystem :: Ord a => [(a,[a])] -> [a] -> LSystem a
lsystem trans st = LSystem (fromList trans) st

-- |Step an LSystem once
step :: Ord a => LSystem a -> LSystem a
step ls@(LSystem trans ts) = ls { state = concatMap myFind ts }
  where myFind c = findWithDefault [c] c trans

-- |Step an LSystem N times
stepN :: Ord a => Int -> LSystem a -> LSystem a
stepN n = foldr (.) id [step | i <- [1..n] ]

-- |Step an LSystem once and return only the resulting sequence
step' :: Ord a => LSystem a -> [a]
step' = state . step

-- |Step an LSystem N times and return only the resulting sequence
stepN' :: Ord a => Int -> LSystem a -> [a]
stepN' n = state . (stepN n)
