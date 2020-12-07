
import Data.String (unwords)
import Data.List (intersperse)

main :: IO ()
main = interact ( display . process . parse)

parse :: String -> [Int]
parse input = map read $ lines input

process :: [Int] -> [Int]
--part 1
--process x = head [ [a, b] | a <- x, b <- x, a + b == 2020, a /= b ]

--part 2
process x = head [
                 [a, b, c] |
                 a <- x, b <- x, c <- x,
                 a + b + c == 2020,
                 a /= b, a /= c, b /=c
                 ]


display :: [Int] -> String
display i = unwords $ (intersperse " * " $ map show i) ++
                      [" = ", show $ foldl (*) 1 i ]

