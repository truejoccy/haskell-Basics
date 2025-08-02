module Main where

-- HC2T1 - Task 1: Checking Types in GHCi
-- Expected Types (before checking in GHCi):
-- 42             :: Num a => a         (usually defaults to Int)
-- 3.14           :: Fractional a => a  (usually defaults to Double)
-- "Haskell"      :: [Char]             (String)
-- 'Z'            :: Char
-- True && False  :: Bool

task1 :: IO ()
task1 = do
  putStrLn "Task 1: Type checking"
  print (42 :: Int)
  print (3.14 :: Double)
  print ("Haskell" :: String)
  print ('Z' :: Char)
  print (True && False)

-- HC2T2 - Task 2: Function Type Signatures and Implementations

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

task2 :: IO ()
task2 = do
  putStrLn "Task 2: Function Signatures and Implementations"
  print (add 5 7)
  print (isEven 4)
  print (concatStrings "Hello, " "World!")

-- HC2T3 - Task 3: Immutable Variables

myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hi there!"

isHaskellFun :: Bool
isHaskellFun = True

task3 :: IO ()
task3 = do
  putStrLn "Task 3: Immutable Variables"
  print myAge
  print piValue
  print greeting
  print isHaskellFun
  -- Uncommenting below will cause a compile-time error
  -- myAge = 30 -- Not allowed in Haskell (immutability)

-- HC2T4 - Task 4: Infix and Prefix Notation

task4 :: IO ()
task4 = do
  putStrLn "Task 4: Converting Between Infix and Prefix Notations"
  -- Infix to Prefix
  print ((+) 5 3)
  print ((*) 10 4)
  print ((&&) True False)
  -- Prefix to Infix
  print (7 + 2)
  print (6 * 5)
  print (True && False)

-- HC2T5 - Task 5: Defining and Using Functions

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

task5 :: IO ()
task5 = do
  putStrLn "Task 5: Using Functions"
  print (circleArea 5.0)
  print (maxOfThree 3 9 7)

-- HC2T6 - Task 6: Understanding Int vs Integer

smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

task6 :: IO ()
task6 = do
  putStrLn "Task 6: Int vs Integer"
  print smallNumber
  print bigNumber
  -- 2^64 is too large for Int on many systems
  print (2 ^ 64 :: Integer)
  -- Uncommenting the next line may cause overflow or error
  -- print (2 ^ 64 :: Int)

-- HC2T7 - Task 7: Boolean Expressions

task7 :: IO ()
task7 = do
  putStrLn "Task 7: Boolean Expressions"
  print (True && True)         -- True using &&
  print (False || False)       -- False using ||
  print (not False)            -- True using not
  print (5 > 10)               -- False comparison

-- Main Function: Calls all tasks

main :: IO ()
main = do
  task1
  task2
  task3
  task4
  task5
  task6
  task7
