--------------------------------------------------------
-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation
--------------------------------------------------------

hc15t1 :: FilePath -> IO ()
hc15t1 path = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e  -> putStrLn $ "Error reading file: " ++ show e
    Right contents -> do
      let linesOfFile = lines contents
      putStrLn "Enter time (s): "
      timeInput <- getLine
      case readMaybe timeInput of
        Nothing -> putStrLn "Invalid time input!"
        Just time -> case linesOfFile of
          (d:_) -> case readMaybe d of
            Nothing -> putStrLn "Invalid distance in file!"
            Just distance -> putStrLn $ "Velocity: " ++ show (distance / time) ++ " m/s"
          _ -> putStrLn "File is empty!"

main1 :: IO ()
main1 = hc15t1 "distance.txt"

--------------------------------------------------------
-- HC15T2: Self-Driving AI Car System
--------------------------------------------------------

reactToLight :: String -> String
reactToLight light = case light of
  "red"    -> "Stop"
  "green"  -> "Go"
  "yellow" -> "Slow down"
  _        -> "Unknown signal"

main2 :: IO ()
main2 = do
  putStrLn "Enter traffic light color:"
  color <- getLine
  putStrLn $ "Action: " ++ reactToLight color

--------------------------------------------------------
-- HC15T3: Custom Exception for Traffic Light Errors
--------------------------------------------------------

data TrafficLightException = InvalidLight String
  deriving (Show, Typeable)

instance Exception TrafficLightException

checkLight :: String -> IO ()
checkLight light = case light of
  "red" -> putStrLn "Stopping."
  "green" -> putStrLn "Going."
  "yellow" -> putStrLn "Slowing down."
  _ -> throwIO (InvalidLight light)

main3 :: IO ()
main3 = do
  putStrLn "Enter traffic light color:"
  color <- getLine
  checkLight color

--------------------------------------------------------
-- HC15T4: Exception Handler for Traffic Light
--------------------------------------------------------

main4 :: IO ()
main4 = do
  putStrLn "Enter traffic light color:"
  color <- getLine
  checkLight color `catch` \(InvalidLight l) -> putStrLn $ "Caught invalid light: " ++ l

--------------------------------------------------------
-- HC15T5: Safe Division Using Maybe
--------------------------------------------------------

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

main5 :: IO ()
main5 = do
  putStrLn "Enter numerator:"
  n <- getLine
  putStrLn "Enter denominator:"
  d <- getLine
  case (readMaybe n, readMaybe d) of
    (Just num, Just denom) -> case safeDivide num denom of
      Just result -> putStrLn $ "Result: " ++ show result
      Nothing     -> putStrLn "Division by zero!"
    _ -> putStrLn "Invalid number input."

--------------------------------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
--------------------------------------------------------

main6 :: IO ()
main6 = do
  putStrLn "Enter an integer:"
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just n -> putStrLn $ "Parsed number: " ++ show n
    Nothing -> putStrLn "Invalid integer input!"

--------------------------------------------------------
-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
--------------------------------------------------------

calcVelocity :: String -> String -> Maybe Double
calcVelocity dStr tStr = do
  d <- readMaybe dStr
  t <- readMaybe tStr
  if t == 0 then Nothing else Just (d / t)

main7 :: IO ()
main7 = do
  putStrLn "Enter distance:"
  d <- getLine
  putStrLn "Enter time:"
  t <- getLine
  case calcVelocity d t of
    Just v  -> putStrLn $ "Velocity: " ++ show v ++ " m/s"
    Nothing -> putStrLn "Error: Invalid input or division by zero."

--------------------------------------------------------
-- HC15T8: Division with Either for Detailed Errors
--------------------------------------------------------

safeDivideEither :: Double -> Double -> Either String Double
safeDivideEither _ 0 = Left "Error: Cannot divide by zero."
safeDivideEither x y = Right (x / y)

main8 :: IO ()
main8 = do
  putStrLn "Enter numerator:"
  n <- getLine
  putStrLn "Enter denominator:"
  d <- getLine
  case (readMaybe n, readMaybe d) of
    (Just num, Just denom) -> case safeDivideEither num denom of
      Right result -> putStrLn $ "Result: " ++ show result
      Left err     -> putStrLn err
    _ -> putStrLn "Error: Invalid number input."

--------------------------------------------------------
-- HC15T9: Try Function for File IO Exceptions
--------------------------------------------------------

main9 :: IO ()
main9 = do
  putStrLn "Enter file name to read:"
  file <- getLine
  content <- try (readFile file) :: IO (Either IOException String)
  case content of
    Left err -> putStrLn $ "File error: " ++ show err
    Right txt -> putStrLn $ "File content:\n" ++ txt

--------------------------------------------------------
-- HC15T10: Hybrid Error Handling with Either and IO
--------------------------------------------------------

readVelocityFromFile :: FilePath -> IO (Either String Double)
readVelocityFromFile file = do
  result <- try (readFile file) :: IO (Either IOException String)
  case result of
    Left e -> return $ Left ("File IO Error: " ++ show e)
    Right contents ->
      case lines contents of
        (d:t:_) -> case (readMaybe d, readMaybe t) of
          (Just dist, Just time) ->
            if time == 0
              then return $ Left "Division by zero."
              else return $ Right (dist / time)
          _ -> return $ Left "Parsing error."
        _ -> return $ Left "File doesn't contain enough lines."

main10 :: IO ()
main10 = do
  putStrLn "Enter file name with distance and time:"
  file <- getLine
  result <- readVelocityFromFile file
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right v  -> putStrLn $ "Velocity: " ++ show v ++ " m/s"
