import System.Environment
import Data.Ix

main :: IO ()
main = do
    args <- getArgs
    if isValidString args
    then do
        let numString = head args
        putStrLn $ show (getIssuer numString)
        putStrLn $ show (isCardValid numString)
    else putStrLn "Error: Invalid amount of arguments.\n$ validate-card <card number>"

isValidString :: [String] -> Bool
isValidString args = length args == 1

getIssuer :: String -> String
getIssuer numString
    | take 2 numString == "34" || take 2 numString == "37" = "American Express"
    | take 2 numString == "36" = "Diners Club (International)"
    | take 2 numString == "54" = "Diners Club (USA & Canada)"
    | take 2 numString == "65" || take 4 numString == "6011" || inRange (644, 649) (read (take 3 numString) :: Int) || inRange (622126, 622925) (read (take 6 numString) :: Int) = "Discover"
    | inRange (637, 639) (read (take 3 numString) :: Int) = "InstaPayment"
    | inRange (3528, 3589) (read (take 4 numString) :: Int) = "JCB"
    | (read (take 4 numString) :: Int) `elem` [5018, 5020, 5038, 5893, 6304, 6759, 6761, 6762, 6763] = "Maestro"
    | inRange (51, 55) (read (take 2 numString) :: Int) || inRange (222100, 272099) (read (take 6 numString) :: Int) = "MasterCard"
    | inRange (300, 305) (read (take 6 numString) :: Int) = "Diners Club (Carte Blanche)"
    | (read (take 4 numString) :: Int) `elem` [4026, 4508, 4844, 4913, 4917] || (read (take 6 numString) :: Int) == 417500 = "Visa Electron"
    | take 1 numString == "4" = "Visa"
    | otherwise = "Other"

stringToIntArray :: String -> [Int]
stringToIntArray numString = map (read . (:"")) numString :: [Int]

isCardValid :: String -> Bool
isCardValid numString = let arr = stringToIntArray numString
                        in last arr == luhnChecksum arr

luhnChecksum :: [Int] -> Int
luhnChecksum arr =  let doubled = doubleOddIndices (reverse (init arr)) 0
                        reduced = reduceLargeValues doubled 0
                        total = sum reduced
                        in total `mod` 10

doubleOddIndices :: [Int] -> Int -> [Int]
doubleOddIndices arr i
    | i == (length arr) = arr 
    | ((i + 1) `mod` 2) /= 0 = let newValue = (arr !! (fromIntegral i) * 2)
                               in doubleOddIndices (replaceAtIndex (fromIntegral i) newValue arr) (i + 1)
    | otherwise = doubleOddIndices arr (i + 1)

reduceLargeValues :: [Int] -> Int -> [Int]
reduceLargeValues arr i
    | i == (length arr) = arr 
    | arr !! i > 9 =    let newValue = (arr !! i) - 9
                        in reduceLargeValues (replaceAtIndex i newValue arr) (i + 1)
    | otherwise = reduceLargeValues arr (i + 1)

replaceAtIndex :: Int -> a -> [a] -> [a] 
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls