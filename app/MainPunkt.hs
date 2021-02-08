{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack)
import qualified Data.Text.IO as Txt
import NLP.Punkt (split_sentences)

corpus :: Text
corpus = "Inconformados com os resultados do pleito, setores aliancistas iniciaram articulações para impedir a posse dos eleitos, as quais culminariam com a revolução de outubro daquele ano. Pedro Aleixo foi considerado um dos mentores intelectuais do movimento em Minas por sua atuação através da imprensa e de comícios. Ainda em 1960, Campista recebeu uma mensagem de elogio do presidente dos Estados Unidos, Dwight Eisenhower, a qual lhe foi transmitida pelo adido do trabalho da embaixada norte-americana, Mr. Fishburn."

main :: IO ()
main = mapM_ Txt.putStrLn (split_sentences corpus)
