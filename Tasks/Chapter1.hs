-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main1 :: IO ()
main1 = print (doubleThenIncrement 5) -- Output: 11


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

main2 :: IO ()
main2 = print (circleArea 3) -- Output: 28.274333882308138


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main3 :: IO ()
main3 = print (greaterThan18 20) -- Output: True


-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers = map fst

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = reverse . quicksort
  where
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, snd y <= snd x]
                        ++ [x]
                        ++ quicksort [y | y <- xs, snd y > snd x]

topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main4 :: IO ()
main4 = print (getTopThreePlayers [("Alice", 50), ("Bob", 90), ("Charlie", 85), ("Dave", 70)]) 
-- Output: ["Bob","Charlie","Dave"]


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

main5 :: IO ()
main5 = print (take 10 infiniteNumbers) -- Output: [1,2,3,4,5,6,7,8,9,10]


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main6 :: IO ()
main6 = print (addNumbers 3 4) -- Output: 7


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

main7 :: IO ()
main7 = print (fToC 98.6) -- Output: 37.0


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main8 :: IO ()
main8 = print (applyTwice (+2) 3) -- Output: 7
