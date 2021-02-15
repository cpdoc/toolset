-- Executar com ./test conllupath tokenrange


import Conllu.IO
import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char

range :: ReadP (Int,Int)
range = do
    _ <- manyTill get (string "TokenRange=")
    b <- munch isDigit
    char ':'
    e <- munch isDigit
    return (read b, read e)

cwRange :: CW AW -> (Int,Int)
cwRange w = fst $ head $ readP_to_S range $ _misc w

sentRange :: Sent -> (Int,Int)
sentRange s = do
    let b = cwRange $ head $ _words s
        e = cwRange $ last $ _words s
    return (fst b, snd e)



-- main = do
--     args <- getArgs
--     x <- readConllu $ head args -- Gera uma lista de documentos (1 só no caso)
--     let sents = head x -- Lista de sentenças do arquivo




--     print 0