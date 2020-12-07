
import Data.String (unwords)

main :: IO ()
main = interact ( display . process . parse)

parse :: String -> [Int]
parse input = map read $ lines input

process :: [Int] -> (Int,Int)
process x = head [ (a, b) | a <- x, b <- x, a + b == 2020, a /= b ]

display :: (Int,Int) -> String
display (left,right) = unwords [ show left
                               , " * "
                               , show right
                               , " = "
                               , show $ left * right
                               ]

