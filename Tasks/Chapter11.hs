-- HC11T1: Greet the User
mainHC11T1 :: IO ()
mainHC11T1 = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"

-- HC11T2: Count Characters in a Line
mainHC11T2 :: IO ()
mainHC11T2 = do
  putStrLn "Enter a line:"
  line <- getLine
  putStrLn $ "Number of characters: " ++ show (length line)

-- HC11T3: Double a Number
mainHC11T3 :: IO ()
mainHC11T3 = do
  putStrLn "Enter a number:"
  input <- getLine
  let num = read input :: Double
  putStrLn $ "Double the number is: " ++ show (num * 2)

-- HC11T4: Concatenate Two Lines
mainHC11T4 :: IO ()
mainHC11T4 = do
  putStrLn "Enter first line:"
  line1 <- getLine
  putStrLn "Enter second line:"
  line2 <- getLine
  putStrLn $ line1 ++ line2

-- HC11T5: Repeat Until "quit"
mainHC11T5 :: IO ()
mainHC11T5 = do
  putStrLn "Enter something (type 'quit' to stop):"
  loop
  where
    loop = do
      input <- getLine
      if map toLower input == "quit"
        then return ()
        else do
          putStrLn $ "You entered: " ++ input
          loop

-- HC11T6: Uppercase Converter
mainHC11T6 :: IO ()
mainHC11T6 = do
  putStrLn "Enter a line:"
  line <- getLine
  putStrLn $ map toUpper line

-- HC11T7: User Options
mainHC11T7 :: IO ()
mainHC11T7 = do
  putStrLn "Options:\n1. Greet\n2. Double a number\n3. Quit"
  putStrLn "Choose an option (1-3):"
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Enter your name:"
      name <- getLine
      putStrLn $ "Hello, " ++ name ++ "!"
    "2" -> do
      putStrLn "Enter a number:"
      input <- getLine
      let num = read input :: Double
      putStrLn $ "Double: " ++ show (num * 2)
    "3" -> putStrLn "Goodbye!"
    _   -> putStrLn "Invalid choice."

-- HC11T8: Even or Odd Checker
mainHC11T8 :: IO ()
mainHC11T8 = do
  putStrLn "Enter a number:"
  input <- getLine
  let num = read input :: Int
  if even num
    then putStrLn "The number is even."
    else putStrLn "The number is odd."

-- HC11T9: Sum Two Numbers
mainHC11T9 :: IO ()
mainHC11T9 = do
  putStrLn "Enter first number:"
  input1 <- getLine
  putStrLn "Enter second number:"
  input2 <- getLine
  let num1 = read input1 :: Double
      num2 = read input2 :: Double
  putStrLn $ "Sum is: " ++ show (num1 + num2)

-- HC11T10: Reverse User Input
mainHC11T10 :: IO ()
mainHC11T10 = do
  putStrLn "Enter some text:"
  input <- getLine
  putStrLn $ reverse input

main :: IO ()
main = mainHC11T1
