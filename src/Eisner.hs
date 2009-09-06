{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}
module Eisner where 
import Chart
import Data.Function (on)
import FSM
import SemiRing
import qualified Data.Map as M
import Data.Monoid.Multiplicative (times, one) 
import Data.Maybe (catMaybes)

type EisnerChart fsa = Chart (Span fsa) (FSMSemiRing fsa)
--type FSA = SimpleFSA String
--type Word = Symbol FSA 
--type Sentence = [Word]


-- Data structure from p. 12 declarative structure  
data SpanEnd fsa =
    SpanEnd {
      hasParent :: Bool, -- b1 and b2 (does the parent exist in the span, i.e. it's not the head)      
      state :: State fsa, -- q1 and q2
      fsa :: fsa,
      word :: Symbol fsa
} 

instance (WFSA fsa) => Show (SpanEnd fsa) where 
    show end = (show $ hasParent end) ++ (show $ state end) ++ (show $ word end) 

expandSpanEnd sp =  (hasParent sp, state sp)

instance (WFSA fsa) => Eq (SpanEnd fsa) where 
    (==)  = (==) `on` expandSpanEnd

instance (WFSA fsa) => Ord (SpanEnd fsa) where 
    compare = compare `on` expandSpanEnd 


data Span fsa =
    Span {
      simple :: Bool, -- s
      leftEnd :: SpanEnd fsa,
      rightEnd :: SpanEnd fsa
} deriving (Show, Eq, Ord) 


type Item fsa = (Span fsa, FSMSemiRing fsa)

hasParentPair span = 
    (hasParent $ leftEnd span , hasParent $ rightEnd span) 

-- Advances an internal WFSA (equivalent in this model to "adjoining" a new
-- dependency. 
advance :: (WFSA fsa) => SpanEnd fsa -> Symbol fsa -> 
           Maybe (SpanEnd fsa, FSMSemiRing fsa) 
advance headSpan nextWord = do 
    (newState, p) <- transition (fsa headSpan) (state headSpan) nextWord 
    return (headSpan {state = newState}, p) 
 


-- implementations of declarative rules


-- The OptLink Rules take spans with dual head (0,0) and adjoin the head on 
-- one side to the head on the other. 
optLinkL :: (WFSA fsa) => Item fsa -> Maybe (Item fsa)
optLinkL (span, semi) = do
      (False, False) <- Just $ hasParentPair span
      (leftEnd', p) <- advance (leftEnd span) (word $ rightEnd span)   
      return $ (span { simple = True, 
                       leftEnd = leftEnd' },
               p `times` semi)


optLinkR :: (WFSA fsa) => Item fsa -> Maybe (Item fsa)
optLinkR (span, semi) = do 
    (False, False) <- Just $ hasParentPair span
    (rightEnd', p) <- advance (rightEnd span) (word $ leftEnd span)   
    return  $ (span {simple = True, 
                      rightEnd = rightEnd' },
                p `times` semi)

-- Combine rules take a right finished simple span 
-- and merge it with a a left finished span. Producing a new span 
-- that is ready for an optlink adjunction  
combine :: (WFSA s) => Item s -> Item s -> Maybe (Item s)
combine (span1, semi1) (span2, semi2) = 
    if simple span1 && (b2 /= b2') && f1 && f2 then 
        Just $ 
             (Span {simple = False,
                    leftEnd = leftEnd span1,
                    rightEnd = rightEnd span2},
             semi1 `times` semi2)
    else Nothing
        where
          ((_, b2), (b2', _)) =  (hasParentPair span1, hasParentPair span2)
          f1 = isFinal (fsa $ rightEnd span1) $ (state $ rightEnd span1)
          f2 = isFinal (fsa $ leftEnd span2) $ (state $ leftEnd span2)



{-getFSA _ = SimpleFSA {
             fsaInitial = 0,
             fsaFinal = 2,
             fsaTransitionWeights = M.fromList [(0, M.fromList[("hello", 0.5)])],
             fsaTransitions = M.fromList [(0, M.fromList[("hello", 1)])]
}-}

singleEnd :: (WFSA fsa) => fsa -> Symbol fsa -> SpanEnd fsa
singleEnd fsa word =
    SpanEnd {                  
      fsa = fsa, 
      state = initialState $ fsa,
      word = word,
      hasParent = False}

-- Seed 
seed :: (WFSA fsa) => (Symbol fsa -> (fsa, fsa)) -> 
        Symbol fsa -> Symbol fsa -> Item fsa
seed getFSA word1 word2 = 
    (Span {
         leftEnd = singleEnd rightFSA word1,
         rightEnd = singleEnd leftFSA word2,
         simple = False} 
    , one)
    where (_, rightFSA) = getFSA word1
          (leftFSA, _) = getFSA word2

processCell :: (WFSA fsa) => 
               (Symbol fsa -> (fsa, fsa)) -> --todo: fix this 
               [Symbol fsa] -> -- The sentence
               Range -> -- Size of the cell 
               (Range -> [(Span fsa, FSMSemiRing fsa)]) -> -- function from cell to contenst 
               [(Span fsa, FSMSemiRing fsa)] -- contents of the new cell 
processCell state sentence (i, k) chart = catMaybes $ 
    if k-i == 2 then
        let seedCell = seed state (sentence !! i) (sentence !! (i+1))
        in
        [Just seedCell,
         optLinkL seedCell,
         optLinkR seedCell]
    else
        concat $ 
        [let s = combine s1 s2 in
           [s, 
            s >>= optLinkL ,
            s >>= optLinkR ]
               | j  <- [i+1..k-1],
                s2 <- chart (j,k),
                s1 <- chart (i,j)]
        
          
    