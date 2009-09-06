module Chart where 
import SemiRing
import Data.Monoid
import qualified Data.Map as M
import Data.Array

-- a distance from start to finish. Sometimes called a span 
-- but use range here to distinguish Eisner's use of span
type Range = (Int, Int) 


-- TODO: It must be possible to O(n) enumerations and O(1) duplicate check.
-- (for now just use Set and eat the extra log) 

-- S bounds the number of items per cell, must be O(1) 
type Cell sig semi =M.Map sig semi   

type Chart sig semi = Array Range (Cell sig semi) 


-- A basic mono-lingual chart parser. 

chartParse :: (SemiRing semi, Ord sig) => 
              Int -> 
              (Range -> (Range -> [(sig, semi)]) -> [(sig, semi)]) -> 
              Chart sig semi 
chartParse n combine = chart 
    where 
      chart = array ((0,0),(n,n)) 

              [((i,k), M.fromListWith mappend $ combine (i,k) (\i -> M.toList $ chart ! i))
                   | i <- [0 .. n ],
                     k <- [i+1 .. n]]