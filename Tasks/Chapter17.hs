-- HC17T1: Severity Data Type and Semigroup Instance
data Severity = Low | Medium | High | Critical
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Severity where
  (<>) = max

main1 :: IO ()
main1 = do
  print $ Low <> Medium       -- Medium
  print $ High <> Medium      -- High
  print $ Critical <> High    -- Critical

-- HC17T2: Min and Max Newtypes with Semigroup
newtype Min a = Min { getMin :: a } deriving (Show)
newtype Max a = Max { getMax :: a } deriving (Show)

instance (Ord a) => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)

instance (Ord a) => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

main2 :: IO ()
main2 = do
  print $ Min 3 <> Min 5      -- Min 3
  print $ Max 3 <> Max 5      -- Max 5

-- HC17T3: Monoid Instance for Severity
instance Monoid Severity where
  mempty = Low

main3 :: IO ()
main3 = do
  print $ mconcat [Low, Medium, High, Low]  -- High
  print $ mempty <> Critical                -- Critical

-- HC17T4: Monoid Instance for Sum Newtype
newtype Sum' = Sum' { getSum' :: Int } deriving (Show)

instance Semigroup Sum' where
  Sum' x <> Sum' y = Sum' (x + y)

instance Monoid Sum' where
  mempty = Sum' 0

main4 :: IO ()
main4 = do
  print $ mconcat [Sum' 1, Sum' 2, Sum' 3]  -- Sum' 6
  print $ mempty <> Sum' 10                -- Sum' 10

-- HC17T5: combineLists Function
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

main5 :: IO ()
main5 = do
  print $ combineLists [1,2,3] [4,5]   -- [1,2,3,4,5]

-- HC17T6: maxSeverity Function
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

main6 :: IO ()
main6 = do
  print $ maxSeverity [Low, Medium, Critical, High]  -- Critical

-- HC17T7: multiplyProducts Function
newtype Product' = Product' { getProduct' :: Int } deriving (Show)

instance Semigroup Product' where
  Product' x <> Product' y = Product' (x * y)

instance Monoid Product' where
  mempty = Product' 1

multiplyProducts :: [Product'] -> Product'
multiplyProducts = mconcat

main7 :: IO ()
main7 = do
  print $ multiplyProducts [Product' 2, Product' 3, Product' 4]  -- Product' 24

-- HC17T8: foldWithSemigroup Function
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

main8 :: IO ()
main8 = do
  print $ foldWithSemigroup [Sum' 2, Sum' 3, Sum' 4]      -- Sum' 9
  print $ foldWithSemigroup [Max 3, Max 9, Max 2]         -- Max 9

-- HC17T9: Config Data Type and Semigroup Instance
data Config = Config
  { loggingLevel :: Severity
  , timeout :: Int
  , retries :: Int
  } deriving (Show)

instance Semigroup Config where
  Config l1 t1 r1 <> Config l2 t2 r2 =
    Config (max l1 l2) (min t1 t2) (max r1 r2)

main9 :: IO ()
main9 = do
  let c1 = Config Medium 30 3
  let c2 = Config High 20 2
  print $ c1 <> c2   -- Config High 20 3

-- HC17T10: Monoid Instance for Config
instance Monoid Config where
  mempty = Config Low maxBound 0

main10 :: IO ()
main10 = do
  let defaultConfig = mempty
  let userConfig = Config Medium 60 5
  print $ defaultConfig <> userConfig  -- Config Medium 60 5

-- Master main to run all
main :: IO ()
main = do
  putStrLn "-- HC17T1 --"
  main1
  putStrLn "\n-- HC17T2 --"
  main2
  putStrLn "\n-- HC17T3 --"
  main3
  putStrLn "\n-- HC17T4 --"
  main4
  putStrLn "\n-- HC17T5 --"
  main5
  putStrLn "\n-- HC17T6 --"
  main6
  putStrLn "\n-- HC17T7 --"
  main7
  putStrLn "\n-- HC17T8 --"
  main8
  putStrLn "\n-- HC17T9 --"
  main9
  putStrLn "\n-- HC17T10 --"
  main10
