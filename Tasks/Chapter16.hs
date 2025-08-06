-- HC16 Tasks Implementation in Haskell

-- HC16T1: Reverse a String
reverseString :: String -> String
reverseString = reverse

main1 :: IO ()
main1 = do
    let input = "hello"
    putStrLn $ "Reversed: " ++ reverseString input


-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

main2 :: IO ()
main2 = do
    let input = "racecar"
    putStrLn $ input ++ " is palindrome? " ++ show (isPalindrome input)


-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main3 :: IO ()
main3 = do
    let n = 5
    putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n)


-- HC16T4: Filter Even Numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even

main4 :: IO ()
main4 = do
    let lst = [1..10]
    putStrLn $ "Even numbers: " ++ show (filterEvens lst)


-- HC16T5: Uppercase String
import Data.Char (toUpper)

toUppercase :: String -> String
toUppercase = map toUpper

main5 :: IO ()
main5 = do
    let input = "hello world"
    putStrLn $ "Uppercased: " ++ toUppercase input


-- HC16T6: nth Fibonacci Number
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main6 :: IO ()
main6 = do
    let n = 10
    putStrLn $ "Fibonacci " ++ show n ++ ": " ++ show (fibonacci n)


-- HC16T7: Element Existence in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

main7 :: IO ()
main7 = do
    let item = 5
    let lst = [1..10]
    putStrLn $ "Does " ++ show item ++ " exist? " ++ show (elementExists item lst)


-- HC16T8: Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where
        insert y [] = [y]
        insert y (z:zs)
            | y <= z    = y : z : zs
            | otherwise = z : insert y zs

main8 :: IO ()
main8 = do
    let lst = [5, 3, 8, 1, 2]
    putStrLn $ "Sorted list: " ++ show (insertionSort lst)


-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise   = x : removeDuplicates xs

main9 :: IO ()
main9 = do
    let lst = [1,2,2,3,4,4,5]
    putStrLn $ "Without duplicates: " ++ show (removeDuplicates lst)


-- HC16T10: Character Frequency in String
import qualified Data.Map as Map

charFrequency :: String -> Map.Map Char Int
charFrequency = foldr (\c acc -> Map.insertWith (+) c 1 acc) Map.empty

main10 :: IO ()
main10 = do
    let input = "haskell"
    putStrLn "Character Frequencies:"
    print $ charFrequency input
