module Main where

-- Imports
import System.Directory (listDirectory)
import Data.List (isInfixOf)
import qualified Data.List as DL
import qualified Data.Map as Map
import qualified Data.Map as M

-------------------------------
-- HC13T1: List Files in Directory
-------------------------------
listFilesInDirectory :: IO [FilePath]
listFilesInDirectory = listDirectory "."

testHC13T1 :: IO ()
testHC13T1 = do
  putStrLn "\nHC13T1 - Listing all files in current directory:"
  files <- listFilesInDirectory
  mapM_ putStrLn files

-------------------------------
-- HC13T2: Filter Files by Substring
-------------------------------
filterFilesBySubstring :: String -> IO [FilePath]
filterFilesBySubstring substr = do
  files <- listFilesInDirectory
  return $ filter (isInfixOf substr) files

testHC13T2 :: IO ()
testHC13T2 = do
  putStrLn "\nHC13T2 - Files filtered by substring '.hs':"
  files <- filterFilesBySubstring ".hs"
  mapM_ putStrLn files

-------------------------------
-- HC13T3: Sort and Return Filtered Files
-------------------------------
sortFilteredFiles :: String -> IO [FilePath]
sortFilteredFiles substr = do
  files <- listFilesInDirectory
  return $ DL.sort $ filter (isInfixOf substr) files

testHC13T3 :: IO ()
testHC13T3 = do
  putStrLn "\nHC13T3 - Sorted files filtered by substring '.hs':"
  files <- sortFilteredFiles ".hs"
  mapM_ putStrLn files

-------------------------------
-- HC13T4: SumNonEmpty Module
-- HC13T5: Restrict Module Export List
-------------------------------

-- Simulated private helper (not exported)
safeSum :: [Int] -> Either String Int
safeSum [] = Left "sumNonEmpty: empty list not allowed"
safeSum xs = Right (sum xs)

-- Public function (would be exported in real module)
sumNonEmpty :: [Int] -> Int
sumNonEmpty xs = case safeSum xs of
  Left err -> error err
  Right total -> total

testHC13T4_HC13T5 :: IO ()
testHC13T4_HC13T5 = do
  putStrLn "\nHC13T4/HC13T5 - Testing sumNonEmpty:"
  print (sumNonEmpty [10, 20, 30])
  -- Uncomment to test error case:
  -- print (sumNonEmpty [])

-------------------------------
-- HC13T6: File Names to Map
-------------------------------
fileNamesToMap :: [FilePath] -> Map.Map Int FilePath
fileNamesToMap files = Map.fromList $ zip [1..] files

testHC13T6 :: IO ()
testHC13T6 = do
  putStrLn "\nHC13T6 - Convert filtered files to Map:"
  files <- filterFilesBySubstring ".hs"
  let fileMap = fileNamesToMap files
  mapM_ print (Map.toList fileMap)

-------------------------------
-- HC13T7: Use Custom Module in Main
-------------------------------
testHC13T7 :: IO ()
testHC13T7 = do
  putStrLn "\nHC13T7 - Using sumNonEmpty from simulated module:"
  let nums = [5, 15, 25]
  print (sumNonEmpty nums)

-------------------------------
-- HC13T8: Qualified Imports for Name Conflicts
-------------------------------
conflictExample :: [Int] -> Int
conflictExample xs = M.size $ M.fromList $ zip xs xs

testHC13T8 :: IO ()
testHC13T8 = do
  putStrLn "\nHC13T8 - Qualified import conflict resolution:"
  print (conflictExample [1,2,3,4])

-------------------------------
-- HC13T9: Renaming Module Namespace
-------------------------------
renamedSort :: [String] -> [String]
renamedSort = DL.sort

renamedMapSize :: Map.Map Int String -> Int
renamedMapSize = M.size

testHC13T9 :: IO ()
testHC13T9 = do
  putStrLn "\nHC13T9 - Using renamed module namespaces:"
  let sorted = renamedSort ["banana", "apple", "carrot"]
  putStrLn $ "Sorted: " ++ show sorted
  let m = M.fromList [(1, "one"), (2, "two")]
  putStrLn $ "Map size: " ++ show (renamedMapSize m)

-------------------------------
-- HC13T10: Multi-Module Main Function
-------------------------------
multiModuleSearchAndDisplay :: String -> IO ()
multiModuleSearchAndDisplay substr = do
  putStrLn $ "\nHC13T10 - Search and display sorted files containing: " ++ substr
  files <- sortFilteredFiles substr
  mapM_ putStrLn files

testHC13T10 :: IO ()
testHC13T10 = multiModuleSearchAndDisplay ".hs"

-------------------------------
-- MAIN: Run All Tests
-------------------------------
main :: IO ()
main = do
  testHC13T1
  testHC13T2
  testHC13T3
  testHC13T4_HC13T5
  testHC13T6
  testHC13T7
  testHC13T8
  testHC13T9
  testHC13T10
