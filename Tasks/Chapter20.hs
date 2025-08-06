-- HC20T1: safeDivide with Maybe Monad
safeDivide :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

main1 = print $ safeDivide 10 2

-- HC20T2: sequenceMaybe for List of Maybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = foldr (\mx acc -> do x <- mx; xs <- acc; return (x:xs)) (Just [])

main2 = print $ sequenceMaybe [Just 1, Just 2, Just 3]

-- HC20T3: Writer Monad Logging Calculator
addW :: Int -> Int -> Writer [String] Int
addW x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

main3 = print $ runWriter (addW 3 4)

-- HC20T4: countChars with State Monad
countChars :: Char -> String -> Int
countChars c = execState (mapM_ (modify . inc) )
  where inc x st = if x == c then st + 1 else st

main4 = print $ countChars 'a' "banana"

-- HC20T5: Reader Monad for Configurable Greeting
type Config = String
greet :: Reader Config String
greet = do
  name <- ask
  return $ "Hello, " ++ name ++ "!"

main5 = print $ runReader greet "Alice"

-- HC20T6: doubleMonad Combining Maybe and List
doubleMonad :: Maybe [a] -> [Maybe a]
doubleMonad Nothing = []
doubleMonad (Just xs) = map Just xs

main6 = print $ doubleMonad (Just [1,2,3])

-- HC20T7: findFirst with Either Monad
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "Not found"
findFirst p (x:xs) = if p x then Right x else findFirst p xs

main7 = print $ findFirst (> 5) [1,2,3,6]

-- HC20T8: Parser Monad for Simple Expressions
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (a, s') <- runParser p s
    return (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  pf <*> pa = Parser $ \s -> do
    (f, s') <- runParser pf s
    (a, s'') <- runParser pa s'
    return (f a, s'')

instance Monad Parser where
  p >>= f = Parser $ \s -> do
    (a, s') <- runParser p s
    runParser (f a) s'

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | x == c = Just (x, xs)
        f _ = Nothing

digit :: Parser Char
digit = Parser f
  where f (x:xs) | isDigit x = Just (x, xs)
        f _ = Nothing

number :: Parser Int
number = do
  ds <- some digit
  return (read ds)

main8 = print $ runParser number "123abc"

-- HC20T9: replicateMonad with Identity Monad
replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = return (replicate n x)

main9 = print $ runIdentity (replicateMonad 4 'a')

-- HC20T10: Nested StateT and MaybeT Transformer
nestedMonad :: StateT Int (MaybeT IO) String
nestedMonad = do
  modify (+1)
  n <- get
  guard (n < 5)
  return ("Count is: " ++ show n)

main10 = do
  res <- runMaybeT $ runStateT nestedMonad 3
  print res

-- HC20T11: randomWalk with State Monad
type Position = (Int, Int)
randomWalk :: Int -> State StdGen [Position]
randomWalk 0 = return [(0,0)]
randomWalk n = do
  rest <- randomWalk (n-1)
  let (x,y) = head rest
  (dx, g1) <- state (randomR (-1,1))
  (dy, g2) <- state (randomR (-1,1))
  put g2
  return ((x+dx, y+dy):rest)

main11 = do
  g <- getStdGen
  print $ evalState (randomWalk 10) g

-- HC20T12: File Reading with IO Monad
main12 = do
  content <- readFile "test.txt"
  mapM_ putStrLn (lines content)

-- HC20T13: fibonacciMemo with State Monad
type Memo = Map Int Integer
fibMemo :: Int -> State Memo Integer
fibMemo 0 = return 0
fibMemo 1 = return 1
fibMemo n = do
  memo <- get
  case Map.lookup n memo of
    Just val -> return val
    Nothing -> do
      a <- fibMemo (n-1)
      b <- fibMemo (n-2)
      let val = a + b
      modify (Map.insert n val)
      return val

main13 = print $ evalState (fibMemo 20) Map.empty

-- HC20T14: mapMFilter Monadic Map-Filter
mapMFilter :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMFilter f [] = return []
mapMFilter f (x:xs) = do
  mb <- f x
  rest <- mapMFilter f xs
  return $ maybe rest (:rest) mb

main14 = print =<< mapMFilter (\x -> return (if even x then Just (x*2) else Nothing)) [1..10]

-- HC20T15: treeSum with Custom Monad
data Tree a = Leaf a | Node (Tree a) (Tree a)

treeSum :: Tree Int -> Identity Int
treeSum (Leaf x) = return x
treeSum (Node l r) = do
  a <- treeSum l
  b <- treeSum r
  return (a + b)

main15 = print $ runIdentity $ treeSum (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))

-- HC20T16: retryIO with IO Monad
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
  result <- tryIO action
  case result of
    Just val -> return (Just val)
    Nothing -> retryIO (n - 1) action
  where
    tryIO a = fmap Just a `catch` \(_ :: IOError) -> return Nothing

main16 = print =<< retryIO 3 (readFile "missing.txt")

-- HC20T17: validatePassword with Either Monad
validatePassword :: String -> Either String String
validatePassword pwd
  | length pwd < 8 = Left "Too short"
  | not (any isUpper pwd) = Left "Must contain uppercase"
  | not (any isDigit pwd) = Left "Must contain digit"
  | otherwise = Right pwd

main17 = print $ validatePassword "Password1"

-- HC20T18: MaybeT Monad Transformer for User Input
getUserInput :: MaybeT IO String
getUserInput = do
  liftIO $ putStrLn "Enter something:"
  input <- liftIO getLine
  guard (not (null input))
  return input

main18 = do
  res <- runMaybeT getUserInput
  print res

-- HC20T19: Writer Monad-based Logging System
logCall :: Show a => String -> a -> Writer [String] a
logCall fname arg = writer (arg, ["Called " ++ fname ++ " with " ++ show arg])

main19 = print $ runWriter (logCall "foo" 123)

-- HC20T20: batchProcessing with Monadic Bind
batchProcessing :: [IO a] -> IO [a]
batchProcessing = foldr (\a acc -> a >>= \x -> acc >>= \xs -> return (x:xs)) (return [])

main20 = batchProcessing [print "First", print "Second", print "Third"]

-- Main function placeholder
main :: IO ()
main = main1 >> main2 >> main3 >> main4 >> main5 >> main6 >> main7 >> main8 >> main9 >> main10
       >> main11 >> main12 >> main13 >> main14 >> main15 >> main16 >> main17 >> main18 >> main19 >> main20
