{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TreeBank where 
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Array
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Monad (liftM)



data POS = POS String 
         deriving (Show, Read, Eq)

data Word = Word String 
          deriving (Show, Read, Eq)

data NonTerm = NTNamed String | NTStar 
          deriving (Show, Read, Eq)

data Spine = Spine [NonTerm]
           deriving (Show, Eq)

type Sentence = Array Int WordInfo
          
data WordInfo = WordInfo {
      ind    :: Int,
      word   :: Word,
      pos    :: POS,
      adjoinInd :: Int,
      spine  :: Spine,
      into   :: Spine,
      sister :: Bool 
} deriving (Eq)


instance Show WordInfo where 
    show wi = 
        intercalate "\t" $ 
        map (\f -> f wi) 
              [show . ind,
               show . word,
               show . pos,
               show . adjoinInd,
               show . spine]

-- parsing Xavier's tree files with spines

lexer       = P.makeTokenParser haskellDef    
nat      = P.natural lexer

class Parsable a where 
    parser :: Parser a

instance Parsable WordInfo where 
    parser = do 
      n <- nat
      spaces
      word <- parser 
      spaces 
      pos <- parser
      spaces
      adjInd <- nat
      spaces
      into <- parser
      spaces
      spine <- parser
      spaces 
      sister <- nat
      spaces
      anyChar
      return $ WordInfo {
                   ind = fromIntegral n,
                   word = word,
                   pos = pos,
                   adjoinInd = fromIntegral adjInd,
                   spine = spine, 
                   into = into,
                   sister = (sister == 1)
                 }

parseString = manyTill anyChar space

instance Parsable Word where 
    parser = Word `liftM` parseString 

instance Parsable POS where 
    parser = POS `liftM` parseString 

instance Parsable NonTerm where 
    parser =  
      choice [NTNamed `liftM` many1 upper,   
              char '*' >> return NTStar]

instance Parsable Spine where 
    parser = do 
      nonterms <- parser `sepBy` char '+' 
      return $ Spine nonterms

parseWordInfo :: String -> Either ParseError WordInfo 
parseWordInfo = parse parser "" 

-- tests 


