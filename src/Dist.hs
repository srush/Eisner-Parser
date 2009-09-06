module Dist where 

import qualified Data.Map as M
import Data.Map ((!))
--import Control.Monad.Random
--oimport System.Random

-- Counts are observed values from the data 
type Counts a = M.Map a Double 

empty :: Counts a 
empty = M.empty

inc :: (Ord a) =>  a -> Double -> Counts a -> Counts a 
inc = M.insertWith (+) 


-- A finite distribution assigns elements to probabilities 
type Dist a = M.Map a Double

-- A conditional distribution is P(b|a) 
type CondDist a b = M.Map a (Dist b) 

-- What is the probability of a result conditioned on a given context
condProb conddist given result = (conddist ! given) ! result 

-- Simplest way to move from counts to a distribution
normalize :: Counts a -> Dist a 
normalize m = M.map (/ total) m
    where total = sum $ M.elems m



--randomdist ::  (MonadRandom m, Enum a, Bounded a, Ord a) => m (Dist a)
--randomdist =  do
 --   vals <- mapM (\a -> do {r <- getRandom; return (a, r)}) allEnum
 --   return $ M.fromList vals

--randomCondDist ::  (MonadRandom m, Enum a, Bounded a, Ord a, Enum b, Bounded b, Ord b) => m (CondDist a b)
--randomCondDist = do 
 -- dists <- mapM (\a -> do {r <- randomdist; return (a, r)}) allEnum
 -- return $ M.fromList dists
