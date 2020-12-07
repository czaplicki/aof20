import Data.Char (isDigit, isSpace)
import Data.List (filter, takeWhile, dropWhile, head, tail)

main :: IO ()
main = interact ( display . process . parse)

parse :: String -> [(Int,Int,Char,String)]
parse input = map parseExp $ lines input

parseExp :: String -> (Int,Int,Char,String)
parseExp input =
    let (min,   src     ) = takeInt input
        (max,   src'    ) = takeInt  $ tail src
        (char,  src''   ) = takeChar $ tail src'
        pass              = dropWhile (\x -> not (isDigit x)  && not (isSpace x)) src''
    in  (min, max, char, pass)

takeChar :: String -> (Char,String)
takeChar i = (head i, tail i)

takeInt :: String -> (Int,String)
takeInt i = ( read $ takeWhile isDigit i, dropWhile isDigit i)


process :: [(Int,Int,Char,String)] -> [(Int,Int,Char,String,Bool)]
process i = map test i

test :: (Int,Int,Char,String) -> (Int,Int,Char,String,Bool)
test (min', max', char, pass) =
    let count = length $ filter (char ==) pass :: Int
        valid = (count > min')
    in  (min', max', char, pass ++ show count, valid)



display :: [(Int,Int,Char,String,Bool)] -> String
-- display = unlines . (map show)
display = show . length . filter (\(_,_,_,_,v) -> v)
