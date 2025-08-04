-- HC5T1: Using applyTwice
applyThreeTimes :: (Int -> Int) -> Int -> Int
applyThreeTimes f x = f (f (f x))

main1 :: IO ()
main1 = print (applyThreeTimes (+1) 5) -- Output: 8

-- HC5T2: Filtering Odd Numbers
filterOdds :: [Int]
filterOdds = filter odd [1..30]

main2 :: IO ()
main2 = print filterOdds -- Output: [1,3,5,...,29]

-- HC5T3: Checking for Uppercase Letters
hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

main3 :: IO ()
main3 = print (hasUppercaseWord ["hello", "World", "test"]) -- Output: True

-- HC5T4: Using Lambda Functions
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main4 :: IO ()
main4 = print (biggerThan10 11) -- Output: True

-- HC5T5: Partial Application
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

main5 :: IO ()
main5 = print (multiplyByFive 7) -- Output: 35

-- HC5T6: Function Composition
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

main6 :: IO ()
main6 = print (evenSquares [1..10]) -- Output: [4,16,36,64,100]

-- HC5T7: The $ Operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main7 :: IO ()
main7 = print result -- Output: 84

-- HC5T8: Point-Free Style
addFive :: Int -> Int
addFive = (+5)

main8 :: IO ()
main8 = print (addFive 10) -- Output: 15

-- HC5T9: Higher-Order Function to Transform a List
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

main9 :: IO ()
main9 = print (transformList (+1) [1, 2, 3]) -- Output: [3,4,5]

-- HC5T10: Combining Higher-Order Functions
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2)

main10 :: IO ()
main10 = print (anySquareGreaterThan50 [1,5,8]) -- Output: True
