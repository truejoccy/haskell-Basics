-- HC19T1: Applicative Instance for Pair
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f1 f2) <*> (Pair x1 x2) = Pair (f1 x1) (f2 x2)

main1 :: IO ()
main1 = do
    let pf = Pair (+1) (*2)
    let pv = Pair 3 4
    print (pf <*> pv)  -- Output: Pair 4 8


-- HC19T2: addThreeApplicative Function
import Control.Applicative (liftA3)

addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

main2 :: IO ()
main2 = do
    print $ addThreeApplicative (Just 1) (Just 2) (Just 3)   -- Just 6
    print $ addThreeApplicative (Just 1) Nothing (Just 3)    -- Nothing


-- HC19T3: safeProduct for Maybe Int
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequence

main3 :: IO ()
main3 = do
    print $ safeProduct [Just 2, Just 3, Just 4]  -- Just 24
    print $ safeProduct [Just 2, Nothing, Just 4] -- Nothing


-- HC19T4: liftAndMultiply with liftA2
import Control.Applicative (liftA2)

liftAndMultiply :: Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2 (*)

main4 :: IO ()
main4 = do
    print $ liftAndMultiply (Just 3) (Just 4)  -- Just 12
    print $ liftAndMultiply (Just 3) Nothing   -- Nothing


-- HC19T5: applyEffects with <*>
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (io1, io2) = (+) <$> io1 <*> io2

main5 :: IO ()
main5 = do
    let io1 = do putStrLn "Enter first number:"; readLn
    let io2 = do putStrLn "Enter second number:"; readLn
    result <- applyEffects (io1, io2)
    putStrLn $ "Sum: " ++ show result


-- HC19T6: repeatEffect with forever
import Control.Monad (forever)

repeatEffect :: IO () -> IO ()
repeatEffect = forever

main6 :: IO ()
main6 = do
    putStrLn "Repeating message forever (Ctrl+C to stop):"
    repeatEffect (putStrLn "Hello, forever!")


-- HC19T7: conditionalPrint with when
import Control.Monad (when)

conditionalPrint :: Bool -> IO ()
conditionalPrint condition = when condition (putStrLn "Condition is True!")

main7 :: IO ()
main7 = do
    conditionalPrint True   -- Prints
    conditionalPrint False  -- Does nothing


-- HC19T8: discardSecond with <*
discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)

main8 :: IO ()
main8 = do
    result <- discardSecond (putStrLn "First effect" >> return 10)
                            (putStrLn "Second effect" >> return 20)
    putStrLn $ "Result: " ++ show result -- Should be 10


-- HC19T9: pureAndApply Demonstration
pureAndApply :: IO Int
pureAndApply = pure (+5) <*> pure 7

main9 :: IO ()
main9 = do
    result <- pureAndApply
    print result -- Output: 12


-- HC19T10: combineResults for Either
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

main10 :: IO ()
main10 = do
    print $ combineResults (Right 3) (Right 4)        -- Right 7
    print $ combineResults (Left "Error1") (Right 4)  -- Left "Error1"
    print $ combineResults (Right 3) (Left "Error2")  -- Left "Error2"
