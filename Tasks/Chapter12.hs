-- HC12T1: Print a Welcome Message
mainHC12T1 :: IO ()
mainHC12T1 = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

mainHC12T2 :: IO ()
mainHC12T2 = do
  let x = 7
      y = 5
  putStrLn $ "Sum of " ++ show x ++ " and " ++ show y ++ " is " ++ show (addTwoNumbers x y)

-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

mainHC12T3 :: IO ()
mainHC12T3 = do
  let n = 5
  putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n)

-- HC12T4: First 10 Fibonacci Numbers (using recursion)
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

mainHC12T4 :: IO ()
mainHC12T4 = do
  let first10 = map fib [0..9]
  putStrLn "First 10 Fibonacci numbers:"
  print first10

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = let lower = map toLower s in lower == reverse lower

mainHC12T5 :: IO ()
mainHC12T5 = do
  putStrLn "Enter a string to check if it is a palindrome:"
  input <- getLine
  if isPalindrome input
    then putStrLn "It is a palindrome."
    else putStrLn "It is not a palindrome."

-- HC12T6: Sort a List of Integers
mainHC12T6 :: IO ()
mainHC12T6 = do
  putStrLn "Enter integers separated by spaces:"
  line <- getLine
  let nums = map read $ words line :: [Int]
  putStrLn "Sorted list:"
  print (quickSort nums)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted  = quickSort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

mainHC12T7 :: IO ()
mainHC12T7 = do
  let radius = 3.5
  putStrLn $ "Area of circle with radius " ++ show radius ++ " is " ++ show (calculateCircleArea radius)

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

mainHC12T8 :: IO ()
mainHC12T8 = do
  let list1 = [1,3,5,7]
      list2 = [2,4,6,8,10]
  putStrLn $ "Merging " ++ show list1 ++ " and " ++ show list2
  print (mergeLists list1 list2)

-- HC12T9: Read and Print File Content with error handling
mainHC12T9 :: IO ()
mainHC12T9 = do
  putStrLn "Enter filename to read:"
  filename <- getLine
  contentOrError <- catch (readFile filename >>= return . Right) handleError
  case contentOrError of
    Right content -> putStrLn content
    Left err -> putStrLn $ "Error reading file: " ++ err
  where
    handleError :: IOException -> IO (Either String String)
    handleError e = return $ Left (show e)

-- HC12T10: Mathematical Operations Module (defined here)
module MathOperations where

add :: Num a => a -> a -> a
add x y = x + y

subtract' :: Num a => a -> a -> a
subtract' x y = x - y

multiply :: Num a => a -> a -> a
multiply x y = x * y

divide :: Fractional a => a -> a -> Maybe a
divide _ 0 = Nothing
divide x y = Just (x / y)

-- For testing the module within this file (normally would be imported)
mainHC12T10 :: IO ()
mainHC12T10 = do
  putStrLn "Math Operations Demo:"
  putStrLn $ "Add 5 and 3: " ++ show (add 5 3)
  putStrLn $ "Subtract 5 from 10: " ++ show (subtract' 10 5)
  putStrLn $ "Multiply 4 and 7: " ++ show (multiply 4 7)
  case divide 20 4 of
    Just res -> putStrLn $ "Divide 20 by 4: " ++ show res
    Nothing -> putStrLn "Cannot divide by zero"

-- Change this to test different tasks:
main :: IO ()
main = mainHC12T1
-- main = mainHC12T2
-- main = mainHC12T3
-- main = mainHC12T4
-- main = mainHC12T5
-- main = mainHC12T6
-- main = mainHC12T7
-- main = mainHC12T8
-- main = mainHC12T9
-- main = mainHC12T10
