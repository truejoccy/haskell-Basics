import Data.Char (toLower, toUpper)

-- HC18T1: mapToLower Function with fmap
mapToLower :: [Char] -> [Char]
mapToLower = fmap toLower

main1 :: IO ()
main1 = do
    putStrLn "HC18T1:"
    print (mapToLower "HeLLo WoRLd")


-- HC18T2: Functor Instance for Tree
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

main2 :: IO ()
main2 = do
    putStrLn "\nHC18T2:"
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    print tree
    print (fmap (*2) tree)


-- HC18T3: incrementTreeValues Function
incrementTreeValues :: Tree Int -> Tree Int
incrementTreeValues = fmap (+1)

main3 :: IO ()
main3 = do
    putStrLn "\nHC18T3:"
    let tree = Node (Leaf 10) (Node (Leaf 20) (Leaf 30))
    print (incrementTreeValues tree)


-- HC18T4: mapToBits Function
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

main4 :: IO ()
main4 = do
    putStrLn "\nHC18T4:"
    print (mapToBits [True, False, True, True, False])


-- HC18T5: Functor Instance for Either
instance Functor (Either e) where
    fmap _ (Left e) = Left e
    fmap f (Right x) = Right (f x)

main5 :: IO ()
main5 = do
    putStrLn "\nHC18T5:"
    print (fmap (+1) (Right 10 :: Either String Int))
    print (fmap (+1) (Left "Error" :: Either String Int))


-- HC18T6: applyToMaybe Function
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

main6 :: IO ()
main6 = do
    putStrLn "\nHC18T6:"
    print (applyToMaybe (*3) (Just 4))
    print (applyToMaybe (*3) Nothing)


-- HC18T7: fmapTuple Function
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

main7 :: IO ()
main7 = do
    putStrLn "\nHC18T7:"
    print (fmapTuple length ("Key", "Value"))


-- HC18T8: identityLawCheck Function
identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

main8 :: IO ()
main8 = do
    putStrLn "\nHC18T8:"
    print (identityLawCheck (Just 5))
    print (identityLawCheck [1, 2, 3])
    print (identityLawCheck (Right 10 :: Either String Int))
    print (identityLawCheck (Leaf 5 :: Tree Int))


-- HC18T9: compositionLawCheck Function
compositionLawCheck :: (Functor f, Eq (f c)) =>
                       (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

main9 :: IO ()
main9 = do
    putStrLn "\nHC18T9:"
    let f = (*2)
        g = (+3)
    print (compositionLawCheck f g (Just 5))
    print (compositionLawCheck f g [1, 2, 3])
    print (compositionLawCheck f g (Right 7 :: Either String Int))
    print (compositionLawCheck f g (Leaf 9 :: Tree Int))


-- HC18T10: nestedFmap Function
nestedFmap :: (a -> b) -> Maybe [a] -> Maybe [b]
nestedFmap = fmap . fmap

main10 :: IO ()
main10 = do
    putStrLn "\nHC18T10:"
    print (nestedFmap toUpper (Just "hello"))
    print (nestedFmap toUpper Nothing)


-- Run all tests together
main :: IO ()
main = do
    main1
    main2
    main3
    main4
    main5
    main6
    main7
    main8
    main9
    main10
