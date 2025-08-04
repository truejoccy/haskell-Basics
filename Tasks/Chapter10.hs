-- HC10T1: ShowSimple Type Class
class ShowSimple a where
  showSimple :: a -> String

data PaymentMethod = Cash | Card | Crypto deriving (Show)

instance ShowSimple PaymentMethod where
  showSimple Cash   = "Cash"
  showSimple Card   = "Card"
  showSimple Crypto = "Crypto"

main1 :: IO ()
main1 = do
  print $ showSimple Cash
  print $ showSimple Card
  print $ showSimple Crypto


-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

main2 :: IO ()
main2 = do
  print $ sumUp [1,2,3,4,5 :: Int]


-- HC10T3: Comparable Type Class
class Comparable a where
  compareWith :: a -> a -> Ordering

data Blockchain = Bitcoin | Ethereum | Cardano deriving (Show, Eq, Ord)

instance Comparable Blockchain where
  compareWith = compare

main3 :: IO ()
main3 = do
  print $ compareWith Bitcoin Ethereum
  print $ compareWith Cardano Bitcoin
  print $ compareWith Ethereum Ethereum


-- HC10T4: Eq Instance for Box
data Box a = Box a deriving (Show)

instance (Eq a) => Eq (Box a) where
  (Box x) == (Box y) = x == y

main4 :: IO ()
main4 = do
  print $ Box 5 == Box 5
  print $ Box 5 == Box 10
  print $ Box "hi" == Box "hi"
  print $ Box "hi" == Box "hello"


-- HC10T5: ShowDetailed Type Class
class ShowDetailed a where
  showDetailed :: a -> String

data User = User { userId :: Int, userName :: String } deriving (Show)

instance ShowDetailed User where
  showDetailed (User uid name) = "User ID: " ++ show uid ++ ", Name: " ++ name

main5 :: IO ()
main5 = do
  let u = User 1 "Alice"
  putStrLn $ showDetailed u


-- HC10T6: Mutual Recursion in Eq for Blockchain
-- We will redefine Eq for Blockchain with mutual recursion between == and /=

instance Eq Blockchain where
  x == y = not (x /= y)
  x /= y = not (x == y)

-- To avoid infinite loop, we override one of them fully for actual logic
-- Let's override (==) with normal equality:
instance {-# OVERLAPPING #-} Eq Blockchain where
  Bitcoin == Bitcoin = True
  Ethereum == Ethereum = True
  Cardano == Cardano = True
  _ == _ = False
  x /= y = not (x == y)

main6 :: IO ()
main6 = do
  print $ Bitcoin == Bitcoin
  print $ Bitcoin /= Ethereum
  print $ Ethereum == Cardano
  print $ Ethereum /= Ethereum


-- HC10T7: Convertible Type Class
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash = "Cash"
  convert Card = "Card"
  convert Crypto = "Crypto"

main7 :: IO ()
main7 = do
  print $ convert Cash :: String
  print $ convert Crypto :: String


-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq Int where
  compareEquality x y = x == y

main8 :: IO ()
main8 = do
  print $ compareEquality (5 :: Int) 5
  print $ compareEquality (5 :: Int) 6


-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

main9 :: IO ()
main9 = do
  print (minValue :: Int)
  print (maxValue :: Int)


-- HC10T10: Concatenatable Type Class
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

main10 :: IO ()
main10 = do
  putStrLn $ concatWith "Hello, " "World!"
  putStrLn $ concatWith "" "EmptyStart"
  putStrLn $ concatWith "EndEmpty" ""
