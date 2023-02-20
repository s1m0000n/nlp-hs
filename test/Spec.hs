import qualified Data.Text.IO as Tio
import Lib (tokenize, defaultTokenizerConfig)

main :: IO ()
main = Tio.readFile "/home/s1m00n/nlp-hs/lorem.txt" >>= writeFile "/home/s1m00n/nlp-hs/lorem-tok.txt" . foldl (\acc tok -> acc ++ "\n" <> show tok) "" . tokenize defaultTokenizerConfig 
