-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to val = from ++ " -> " ++ to ++ ": " ++ show val

main1 :: IO ()
main1 = putStrLn $ generateTx "Alice" "Bob" 100


-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person2 = Person2
  { name2    :: String
  , address2 :: (String, Int)
  , payment  :: PaymentMethod
  } deriving Show

bob :: Person2
bob = Person2 "Bob" ("123 Street", 45) Cash

main2 :: IO ()
main2 = print bob


-- HC8T3: Algebraic Data Types and Functions
data Shape3 = Circle Float | Rectangle Float Float

area :: Shape3 -> Float
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h

main3 :: IO ()
main3 = do
  print $ area (Circle 5)
  print $ area (Rectangle 10 5)


-- HC8T4: Record Syntax for Employee
data Employee = Employee
  { name :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

main4 :: IO ()
main4 = print richard


-- HC8T5: Record Syntax for Person
data Person5 = Person5
  { name5       :: String
  , age         :: Int
  , isEmployed  :: Bool
  } deriving Show

person1 :: Person5
person1 = Person5 "Alice" 30 True

person2 :: Person5
person2 = Person5 "John" 25
