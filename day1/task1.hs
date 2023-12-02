import Data.Char (isDigit)

-- Function to extract digits from a string
extractDigits :: String -> [Int]
extractDigits = map (\c -> read [c]) . filter isDigit

-- Function to calculate the specified sum for a line
calculateSum :: String -> Int
calculateSum line = case extractDigits line of
    [] -> 0
    [x] -> x * 10 + x  -- Use the digit as both first and last if there's only one
    xs -> head xs * 10 + last xs

-- Main function to process all lines and sum the results
main :: IO ()
main = do
    input <- getContents
    let lineSums = map calculateSum $ lines input
    print $ sum lineSums