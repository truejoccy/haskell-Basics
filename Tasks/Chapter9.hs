-- HC9T1: Define a Parametric Type Synonym
type Entity a = (a, String)  -- (address, description)

mainHC9T1 :: IO ()
mainHC9T1 = do
  let e1 :: Entity Int
      e1 = (12345, "Home Address")
      e2 :: Entity String
      e2 = ("0xAB12CD34", "Wallet Address")
  putStrLn $ "Entity 1: " ++ show e1
  putStrLn $ "Entity 2: " ++ show e2

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)

mainHC9T2 :: IO ()
mainHC9T2 = do
  let b1 = Empty :: Box Int
      b2 = Has 42
  putStrLn $ "Box 1: " ++ show b1
  putStrLn $ "Box 2: " ++ show b2

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty   = Empty
addN n (Has x) = Has (x + n)

mainHC9T3 :: IO ()
mainHC9T3 = do
  let b1 = Has 10
      b2 = Empty :: Box Int
  print $ addN 5 b1   -- Has 15
  print $ addN 5 b2   -- Empty

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x

mainHC9T4 :: IO ()
mainHC9T4 = do
  let b1 = Has "Hello"
      b2 = Empty :: Box String
  putStrLn $ extract "Default" b1
  putStrLn $ extract "Default" b2

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Double }
  | Rectangle { color :: a, width :: Double, height :: Double }
  deriving (Show)

mainHC9T5 :: IO ()
mainHC9T5 = do
  let c = Circle { color = "Red", radius = 10 }
      r = Rectangle { color = "Blue", width = 5, height = 7 }
  print c
  print r

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving (Show)

mainHC9T6 :: IO ()
mainHC9T6 = do
  let t3 = Tweet "Nice pic!" 2 []
      t2 = Tweet "Great post!" 5 [t3]
      t1 = Tweet "Hello world" 10 [t2]
  print t1

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ likes comments) = likes + sum (map engagement comments)

mainHC9T7 :: IO ()
mainHC9T7 = do
  let t3 = Tweet "Nice pic!" 2 []
      t2 = Tweet "Great post!" 5 [t3]
      t1 = Tweet "Hello world" 10 [t2]
  print $ engagement t1  -- Should be 10 + 5 + 2 = 17

-- HC9T8: Recursive Sequence Data Type
data Sequence a = SeqNode a (Sequence a) | SeqEnd deriving (Show)

mainHC9T8 :: IO ()
mainHC9T8 = do
  let seq1 = SeqNode 1 (SeqNode 2 (SeqNode 3 SeqEnd))
  print seq1

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ SeqEnd = False
elemSeq x (SeqNode y rest)
  | x == y    = True
  | otherwise = elemSeq x rest

mainHC9T9 :: IO ()
mainHC9T9 = do
  let seq1 = SeqNode 1 (SeqNode 2 (SeqNode 3 SeqEnd))
  print $ elemSeq 2 seq1  -- True
  print $ elemSeq 5 seq1  -- False

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree | Node a (BST a) (BST a) deriving (Show)

mainHC9T10 :: IO ()
mainHC9T10 = do
  let tree = Node 10
              (Node 5 EmptyTree EmptyTree)
              (Node 15 EmptyTree EmptyTree)
  print tree

-- Optionally, you can create a main function that runs all tests sequentially:
main :: IO ()
main = do
  putStrLn "HC9T1:"
  mainHC9T1
