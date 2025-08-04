-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

main1 :: IO ()
main1 = do
  putStrLn $ weatherReport "sunny"
  putStrLn $ weatherReport "rainy"
  putStrLn $ weatherReport "windy"

-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

main2 :: IO ()
main2 = do
  putStrLn $ dayType "Saturday"
  putStrLn $ dayType "Monday"
  putStrLn $ dayType "Funday"

-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89  = "Good job!"
  | n >= 50 && n <= 69  = "You passed."
  | n >= 0  && n <= 49  = "Better luck next time."
  | otherwise           = "Invalid grade"

main3 :: IO ()
main3 = do
  print $ gradeComment 95
  print $ gradeComment 75
  print $ gradeComment 30
  print $ gradeComment 150

-- HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! Yay!"
specialBirthday 18 = "Adulthood begins!"
specialBirthday 50 = "Half a century!"
specialBirthday _  = "Happy Birthday!"

main4 :: IO ()
main4 = do
  putStrLn $ specialBirthday 1
  putStrLn $ specialBirthday 18
  putStrLn $ specialBirthday 30

-- HC4T5 - Task 5: Add a Catch-All Pattern with a Custom Message
specialBirthdayDetailed :: Int -> String
specialBirthdayDetailed 1  = "First birthday! Yay!"
specialBirthdayDetailed 18 = "Adulthood begins!"
specialBirthdayDetailed 50 = "Half a century!"
specialBirthdayDetailed age = "Happy Birthday! You are " ++ show age ++ " years old."

main5 :: IO ()
main5 = do
  putStrLn $ specialBirthdayDetailed 50
  putStrLn $ specialBirthdayDetailed 33

-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one element."
whatsInsideThisList [_, _]   = "The list has two elements."
whatsInsideThisList _        = "The list has many elements."

main6 :: IO ()
main6 = do
  print $ whatsInsideThisList ([] :: [Int])
  print $ whatsInsideThisList [1]
  print $ whatsInsideThisList [1,2]
  print $ whatsInsideThisList [1,2,3]

-- HC4T7 - Task 7: Ignore Elements in a List
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

main7 :: IO ()
main7 = do
  print $ firstAndThird [1,2,3,4]
  print $ firstAndThird [5,6]
  print $ firstAndThird [7,8,9]

-- HC4T8 - Task 8: Extract Values from Tuples
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) = 
  name ++ " is " ++ show age ++ " years old and " ++ studentStatus
  where
    studentStatus = if isStudent then "a student." else "not a student."

main8 :: IO ()
main8 = do
  putStrLn $ describeTuple ("Alice", 22, True)
  putStrLn $ describeTuple ("Bob", 35, False)
