-- HC3T1 - Task 1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n = if n > 0 then "Positive"
                else if n < 0 then "Negative"
                else "Zero"

main1 :: IO ()
main1 = do
    print (checkNumber 5)
    print (checkNumber (-3))
    print (checkNumber 0)


-- HC3T2 - Task 2: Determine the grade based on a score using guards
grade :: Int -> String
grade n
    | n >= 90 = "A"
    | n >= 80 = "B"
    | n >= 70 = "C"
    | n >= 60 = "D"
    | otherwise = "F"

main2 :: IO ()
main2 = do
    print (grade 95)
    print (grade 72)
    print (grade 50)


-- HC3T3 - Task 3: Convert an RGB color to a hex string using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let toHex x = if x < 16 then '0' : showHex x "" else showHex x ""
    in "#" ++ toHex r ++ toHex g ++ toHex b

import Numeric (showHex)

main3 :: IO ()
main3 = do
    print (rgbToHex (255, 0, 127))
    print (rgbToHex (0, 255, 64))


-- HC3T4 - Task 4: Calculate the area of a triangle using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

main4 :: IO ()
main4 = do
    print (triangleArea 3 4 5)
    print (triangleArea 7 8 9)


-- HC3T5 - Task 5: Determine the type of a triangle using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

main5 :: IO ()
main5 = do
    print (triangleType 3 3 3)
    print (triangleType 5 5 8)
    print (triangleType 6 7 8)


-- HC3T6 - Task 6: Check leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

main6 :: IO ()
main6 = do
    print (isLeapYear 2000)
    print (isLeapYear 1900)
    print (isLeapYear 2024)


-- HC3T7 - Task 7: Determine the season based on the month using guards
season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Winter"
    | m >= 3 && m <= 5 = "Spring"
    | m >= 6 && m <= 8 = "Summer"
    | m >= 9 && m <= 11 = "Autumn"
    | otherwise = "Invalid month"

main7 :: IO ()
main7 = do
    print (season 3)
    print (season 7)
    print (season 11)


-- HC3T8 - Task 8: Calculate BMI and return category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25 = "Normal"
    | bmi < 30 = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / (height ^ 2)

main8 :: IO ()
main8 = do
    print (bmiCategory 70 1.75)
    print (bmiCategory 90 1.8)


-- HC3T9 - Task 9: Find the maximum of three numbers using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = if a > b then a else b
        maxAll = if maxAB > c then maxAB else c
    in maxAll

main9 :: IO ()
main9 = do
    print (maxOfThree 10 20 15)
    print (maxOfThree 5 25 10)


-- HC3T10 - Task 10: Check if a string is a palindrome using recursion and guards
isPalindrome :: String -> Bool
isPalindrome s
    | length s <= 1 = True
    | head s == last s = isPalindrome (init (tail s))
    | otherwise = False

main10 :: IO ()
main10 = do
    print (isPalindrome "racecar")
    print (isPalindrome "haskell")
    print (isPalindrome "madam")
