
import Data.Char (toUpper)


main :: IO ()
main  = interact ( map toUpper )
--main = interact ( parse . process . display)
--parse :: String -> T
--prosses :: T -> V
--display :: V -> String
