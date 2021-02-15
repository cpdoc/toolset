import Conllu.IO
import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char

range :: ReadP (Int,Int) -- Range parser
range = do
    _ <- manyTill get (string "TokenRange=")
    b <- munch isDigit
    char ':'
    e <- munch isDigit
    return (read b, read e)

cwRange :: CW AW -> (Int,Int) -- Take range of element
cwRange w = fst $ head $ readP_to_S range $ _misc w

sentRange :: Sent -> (Int,Int) -- Take tange of sentence
sentRange s = do
    let b = cwRange $ head $ _words s
        e = cwRange $ last $ _words s
    return (fst b, snd e)

isSubrange :: (Int,Int) -> (Int,Int) -> Bool -- Verify if range is subrange of other
isSubrange (b1,e1) (b2,e2) = b1 >= b2 && e1 <= e2

sentFind :: [Sent] -> (Int, Int) -> Maybe Sent -- Find sentence that contains given range
sentFind l t = do
    let ranges = filter (isSubrange t . sentRange) l
    return $ if null ranges then Nothing else Just $ head ranges

isSubTree :: [CW AW] -> Bool -- Check if a list of words is a unique subtree
isSubTree w = do
    let sid = map _id w
        heads = map (_head . fromJust . _rel) w
        outsiders = filter (`notElem` sid) heads
    return $ length outsiders <= 1 

compare :: Sent -> (Int,Int) -> Bool -- Check if an entity token range is consistent with a sentence subtree
compare entSent entRange = do
    let content = filter (\e -> isSubrange (cwRange e) entRange) $ _words entSent
    return isSubTree content

main = do
    [conlluPath,begin,end] <- getArgs
    x <- readConllu conlluPath
    let sents = head x
        entRange = (read begin, read end)
        entSent = sentFind sents entRange
        consistence = compare entSent entRange
    putStrLn $ if consistente then "Esse arquivo é consistente" else "Esse arquivo NÃO é consistente"