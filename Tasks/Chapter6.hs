-- HC6T1: Factorial (Recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = print $ factorial 5  -- Output: 120

----------------------------------------------------------

-- HC6T2: Fibonacci (Recursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = print $ fibonacci 10  -- Output: 55

----------------------------------------------------------

-- HC6T3: Sum of Elements Using foldr
sumFoldr :: [Int] -> Int
sumFoldr = foldr (+) 0

main3 :: IO ()
main3 = print $ sumFoldr [1, 2, 3, 4, 5]  -- Output: 15

----------------------------------------------------------

-- HC6T4: Product of Elements Using foldl
productFoldl :: [Int] -> Int
productFoldl = foldl (*) 1

main4 :: IO ()
main4 = print $ productFoldl [1, 2, 3, 4]  -- Output: 24

----------------------------------------------------------

-- HC6T5: Reverse a List (Recursive)
reverseRec :: [a] -> [a]
reverseRec [] = []
reverseRec (x:xs) = reverseRec xs ++ [x]

main5 :: IO ()
main5 = print $ reverseRec [1, 2, 3, 4]  -- Output: [4,3,2,1]

----------------------------------------------------------

-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs) = (e == x) || elementExists e xs

main6 :: IO ()
main6 = print $ elementExists 3 [1, 2, 3, 4]  -- Output: True

----------------------------------------------------------

-- HC6T7: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

main7 :: IO ()
main7 = print $ listLength [1, 2, 3, 4, 5]  -- Output: 5

----------------------------------------------------------

-- HC6T8: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

main8 :: IO ()
main8 = print $ filterEven [1, 2, 3, 4, 5, 6]  -- Output: [2,4,6]

----------------------------------------------------------

-- HC6T9: Map Implementation
mapFunc :: (a -> b) -> [a] -> [b]
mapFunc _ [] = []
mapFunc f (x:xs) = f x : mapFunc f xs

main9 :: IO ()
main9 = print $ mapFunc (*2) [1, 2, 3]  -- Output: [2,4,6]

----------------------------------------------------------

-- HC6T10: Digits of a Number (Recursive)
digits :: Integer -> [Integer]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main10 :: IO ()
main10 = print $ digits 12345  -- Output: [1,2,3,4,5]

----------------------------------------------------------

-- To run all:
main :: IO ()
main = do
    main1
    main2
    main3
    main4
    main5
    main6
    main7
    main8
    main9
    main10
