-- HC7T1: Define Color and Eq instance
data Color = Red | Green | Blue
  deriving (Show)  -- Show for convenience

instance Eq Color where
  Red == Red     = True
  Green == Green = True
  Blue == Blue   = True
  _ == _         = False

-- HC7T2: Ord instance for Color (Red < Green < Blue)
instance Ord Color where
  Red   <= _     = True
  Green <= Green = True
  Green <= Blue  = True
  Blue  <= Blue  = True
  _     <= _     = False

-- HC7T3: Function compareValues with Eq and Ord constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Shape type with Show and Read
data Shape = Circle Double | Rectangle Double Double
  deriving (Eq)

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input = 
    case words input of
      ("Circle":rStr:rest) -> 
        case reads rStr of
          [(r,"")] -> [(Circle r, unwords rest)]
          _        -> []
      ("Rectangle":wStr:hStr:rest) -> 
        case (reads wStr, reads hStr) of
          ([(w,"")], [(h,"")]) -> [(Rectangle w h, unwords rest)]
          _                    -> []
      _ -> []

-- HC7T5: squareArea with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: circleCircumference working with Integral and Floating
circleCircumference :: (Integral a, Floating b) => a -> b
circleCircumference r = 2 * pi * fromIntegral r

-- HC7T7: nextColor using Bounded and Enum
instance Enum Color where
  fromEnum Red   = 0
  fromEnum Green = 1
  fromEnum Blue  = 2

  toEnum 0 = Red
  toEnum 1 = Green
  toEnum 2 = Blue
  toEnum _ = error "Invalid Color Enum"

instance Bounded Color where
  minBound = Red
  maxBound = Blue

nextColor :: Color -> Color
nextColor c 
  | c == maxBound = minBound
  | otherwise     = succ c

-- HC7T8: parseShape returning Maybe Shape
parseShape :: String -> Maybe Shape
parseShape str = 
  case reads str of
    [(shape, "")] -> Just shape
    _             -> Nothing

-- HC7T9: Describable type class with instances for Bool and Shape
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is True"
  describe False = "This is False"

instance Describable Shape where
  describe (Circle r)      = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

-- HC7T10: describeAndCompare for Describable values with Ord and Eq constraints
describeAndCompare :: (Describable a, Eq a, Ord a) => a -> a -> String
describeAndCompare x y = describe $ compareValues x y

-- Main functions for testing (only one active at a time)

-- HC7T1 Test
mainHC7T1 :: IO ()
mainHC7T1 = do
  print (Red == Red)      -- True
  print (Red == Green)    -- False
  print (Blue == Blue)    -- True

-- HC7T2 Test
mainHC7T2 :: IO ()
mainHC7T2 = do
  print (Red < Green)     -- True
  print (Green < Blue)    -- True
  print (Blue < Red)      -- False

-- HC7T3 Test
mainHC7T3 :: IO ()
mainHC7T3 = do
  print $ compareValues 5 3
  print $ compareValues 'a' 'z'

-- HC7T4 Test
mainHC7T4 :: IO ()
mainHC7T4 = do
  let c = Circle 3.5
  let r = Rectangle 4 5
  print c
  print r
  print (read "Circle 7.2" :: Shape)
  print (read "Rectangle 2.0 3.0" :: Shape)

-- HC7T5 Test
mainHC7T5 :: IO ()
mainHC7T5 = do
  print $ squareArea (5 :: Int)
  print $ squareArea (3.2 :: Double)

-- HC7T6 Test
mainHC7T6 :: IO ()
mainHC7T6 = do
  print $ circleCircumference (5 :: Int)
  print $ circleCircumference (10 :: Integer)

-- HC7T7 Test
mainHC7T7 :: IO ()
mainHC7T7 = do
  print $ nextColor Red
  print $ nextColor Green
  print $ nextColor Blue

-- HC7T8 Test
mainHC7T8 :: IO ()
mainHC7T8 = do
  print $ parseShape "Circle 5.0"
  print $ parseShape "Rectangle 4.0 3.0"
  print $ parseShape "Triangle 3 4 5"  -- Should be Nothing

-- HC7T9 Test
mainHC7T9 :: IO ()
mainHC7T9 = do
  print $ describe True
  print $ describe False
  print $ describe (Circle 4.5)
  print $ describe (Rectangle 2 3)

-- HC7T10 Test
-- Need Ord and Eq instances for Shape
-- Let's derive them (using area as a rough measure for comparison)

instance Ord Shape where
  compare (Circle r1) (Circle r2) = compare r1 r2
  compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1*h1) (w2*h2)
  compare (Circle r) (Rectangle w h) = compare (pi*r*r) (w*h)
  compare (Rectangle w h) (Circle r) = compare (w*h) (pi*r*r)

mainHC7T10 :: IO ()
mainHC7T10 = do
  let s1 = Circle 3
  let s2 = Rectangle 4 4
  putStrLn $ describeAndCompare s1 s2

-- To test any particular main, uncomment the corresponding line below:
main = mainHC7T1
-- main = mainHC7T2
-- main = mainHC7T3
-- main = mainHC7T4
-- main = mainHC7T5
-- main = mainHC7T6
-- main = mainHC7T7
-- main = mainHC7T8
-- main = mainHC7T9
-- main = mainHC7T10
