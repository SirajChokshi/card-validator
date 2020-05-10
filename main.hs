import System.Environment
import Data.Ix

main :: IO ()
main = do
    args <- getArgs
    if isValidString args
    then do
        let numString = head args
        putStrLn $ show (getIssuer numString)
    else putStrLn "Error: Invalid amount of arguments.\n$ validate-card <card number>"

isValidString :: [String] -> Bool
isValidString args = length args == 1 && length (head args) >= 13

getIssuer :: String -> String
getIssuer numString
    | take 2 numString == "34" || take 2 numString == "37" = "American Express"
    | take 2 numString == "36" = "Diners Club (International)"
    | take 2 numString == "54" = "Diners Club (USA & Canada)"
    | take 2 numString == "65" || take 4 numString == "6011" || inRange (644, 649) (read (take 3 numString) :: Integer) || inRange (622126, 622925) (read (take 6 numString) :: Integer) = "Discover"
    | inRange (637, 639) (read (take 3 numString) :: Integer) = "InstaPayment"
    | inRange (3528, 3589) (read (take 4 numString) :: Integer) = "JCB"
    | (read (take 4 numString) :: Integer) `elem` [5018, 5020, 5038, 5893, 6304, 6759, 6761, 6762, 6763] = "Maestro"
    | inRange (51, 55) (read (take 2 numString) :: Integer) || inRange (222100, 272099) (read (take 6 numString) :: Integer) = "MasterCard"
    | inRange (300, 305) (read (take 6 numString) :: Integer) = "Diners Club (Carte Blanche)"
    | (read (take 4 numString) :: Integer) `elem` [4026, 4508, 4844, 4913, 4917] || (read (take 6 numString) :: Integer) == 417500 = "Visa Electron"
    | take 1 numString == "4" = "Visa"
    | otherwise = "Other"
