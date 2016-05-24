module Application where

import           ClassyPrelude                 hiding (many, try)
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List                     ((!!))
import           Text.ParserCombinators.Parsec hiding ((<|>))

data Unown = Angry
           | Bear
           | Chase
           | Direct   Unown
           | Engage
           | Find
           | Give     Unown
           | Help
           | Increase Unown
           | Join     Unown Unown
           | Keep     Unown Unown
           | Laugh
           | Make
           | Nuzzle
           | Observe  Unown
           | Perform  Unown Unown
           | Quicken  Unown Unown
           | Reassure
           | Search
           | Tell     Unown
           | Undo
           | Vanish
           | Want
           | XXXXX    Unown Unown
           | Yield
           | Zoom     Unown Unown
           | Exclaim  Unown Unown
           | Question
    deriving (Eq, Ord, Read, Show)

data UnownValue = UnownBool Bool | UnownInt Int | UnownString String | UnownList [UnownValue]
    deriving (Eq, Ord, Read, Show)

type UnownStateIO a = StateT [a] IO a

appMain :: IO ()
appMain = hGetContents stdin >>= exec

exec :: String -> IO ()
exec = either print (\r -> evalStateT (eval r) [] >> putStrLn "") . readExpr

readExpr :: String -> Either ParseError Unown
readExpr = parse parseExpr "Unown"

parseExpr :: Parser Unown
parseExpr = spaces *> (parsePrefix >>= (\l -> parseInfixR l <|> return l))

parsePrefix :: Parser Unown
parsePrefix = many letter <* spaces >>= c
  where c ('d' : _) = Direct   <$> parsePrefix
        c ('g' : _) = Give     <$> parsePrefix
        c ('i' : _) = Increase <$> parsePrefix
        c ('j' : _) = Join     <$> parsePrefix <*> parsePrefix
        c ('k' : _) = Keep     <$> parsePrefix <*> parsePrefix
        c ('m' : _) = return       Make
        c ('o' : _) = Observe  <$> parsePrefix
        c ('p' : _) = Perform  <$> parsePrefix <*> parsePrefix
        c ('q' : _) = Quicken  <$> parsePrefix <*> parsePrefix
        c ('t' : _) = Tell     <$> parsePrefix
        c ('x' : _) = XXXXX    <$> parsePrefix <*> parsePrefix
        c ('z' : _) = Zoom     <$> parsePrefix <*> parsePrefix
        c        _  = unexpected "prefix"

parseInfixR :: Unown -> Parser Unown
parseInfixR l = char '!' *> spaces *> (Exclaim l <$> parseExpr)

eval :: Unown -> UnownStateIO UnownValue
eval (Direct   u    ) = unownConvertString <$> eval u
eval (Give     u    ) = eval u >>= modify . (:) >> gets headEx
eval (Increase u    ) = (\(UnownInt i) -> UnownInt $ i + 1) <$> eval u
eval (Join     u1 u2) = unownSum <$> eval u1 <*> eval u2
eval (Keep     pr co) = eval pr >>= c
  where c (UnownBool True) = (\h (UnownList t) -> UnownList $ h : t) <$>
            eval co <*> eval (Keep pr co)
        c _                = return $ UnownList []
eval (Make          ) = return $ UnownInt 0
eval (Observe  u    ) = eval u >>= (\(UnownInt i) -> gets (!! i))
eval (Perform  u1 u2) = unownProduct <$> eval u1 <*> eval u2
eval (Quicken  u1 u2) = (\(UnownInt i1) (UnownInt i2) -> UnownInt $ i1 ^ i2) <$> eval u1 <*> eval u2
eval (Tell     u    ) = eval u >>= (\r -> liftIO (putStr $ unownRawShow r) >> return r)
eval (XXXXX    u1 u2) = eval u1 >> eval u2
eval (Zoom     u1 u2) = UnownBool <$> ((<) <$> eval u1 <*> eval u2)
eval (Exclaim  u1 u2) = eval $ XXXXX u1 u2
eval _                = error "not impl"

unownSum :: UnownValue -> UnownValue -> UnownValue
unownSum (UnownBool a) (UnownBool b) = UnownBool $ a || b
unownSum (UnownInt  a) (UnownInt  b) = UnownInt  $ a +  b
unownSum (UnownList a) (UnownList b) = UnownList $ a <> b
unownSum _ _ = error "sum pattern is illegal"

unownProduct :: UnownValue -> UnownValue -> UnownValue
unownProduct (UnownBool a) (UnownBool b) = UnownBool $ a && b
unownProduct (UnownInt  a) (UnownInt  b) = UnownInt  $ a *  b
unownProduct (UnownList a) (UnownList b) = UnownList $ zipWith (\c d -> UnownList [c, d]) a b
unownProduct _ _ = error "product pattern is illegal"

unownConvertString :: UnownValue -> UnownValue
unownConvertString (UnownInt  x) = UnownString $ singleton $ chr x
unownConvertString (UnownList x) = foldr (unownSum) (UnownList []) $ map unownConvertString x
unownConvertString _ = error "convert string pattern is illegal"

unownRawShow :: UnownValue -> Text
unownRawShow (UnownBool   x) = tshow x
unownRawShow (UnownInt    x) = tshow x
unownRawShow (UnownString x) = pack  x
unownRawShow (UnownList   x) = tshow x
