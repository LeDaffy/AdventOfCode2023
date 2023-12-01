import Data.Maybe
charToInt :: Char -> Maybe Integer
charToInt c
    | c == '0' = Just 0
    | c == '1' = Just 1
    | c == '2' = Just 2
    | c == '3' = Just 3
    | c == '4' = Just 4
    | c == '5' = Just 5
    | c == '6' = Just 6
    | c == '7' = Just 7
    | c == '8' = Just 8
    | c == '9' = Just 9
    | otherwise = Nothing

parseNumbers :: [Char] -> [Maybe Integer]
parseNumbers = map charToInt

parseFirstNum :: [Char] -> [Maybe Integer]
parseFirstNum str = takeWhile isJust $ dropWhile isNothing (parseNumbers str)

parseLastNum :: [Char] -> [Maybe Integer]
parseLastNum str = reverse $ parseFirstNum $ reverse str

getFirstDigit :: [Char] -> Integer
getFirstDigit str = head $ map fromJust (parseFirstNum str)

getLastDigit :: [Char] -> Integer
getLastDigit str = last $ map fromJust (parseLastNum str)

parseCode :: [Char] -> Integer
parseCode str = getFirstDigit str * 10 + getLastDigit str

