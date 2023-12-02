import Data.List (isPrefixOf, maximumBy, minimumBy, tails, findIndex)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)

numberWords :: [(String, Int)]
numberWords = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), 
               ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9),
               ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), 
               ("6", 6), ("7", 7), ("8", 8), ("9", 9)]

findFirstNumber :: String -> Int
findFirstNumber str = 
    let matchPositions = mapMaybe (\(word, num) -> fmap (\idx -> (idx, num)) $ findIndex (isPrefixOf word) (tails str)) numberWords
    in if null matchPositions then 0 else snd $ minimumBy (comparing fst) matchPositions

findLastNumber :: String -> Int
findLastNumber str = 
    let matchPositions = mapMaybe (\(word, num) -> fmap (\idx -> (idx, num)) $ findIndex (isPrefixOf word) (tails str)) numberWords
    in if null matchPositions then 0 else snd $ maximumBy (comparing fst) matchPositions

calculateSum :: String -> Int
calculateSum line = (findFirstNumber line)*10 + findLastNumber line

main :: IO ()
main = do
    contents <- getContents
    let linesOfInput = lines contents
        sums = map calculateSum linesOfInput
        totalSum = sum sums
    print totalSum