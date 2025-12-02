module Day2 where

import Data.List.Split
import Data.List

dc n = length $ show n

(***) :: Int -> Int -> Int
a *** b
 | b == 0 = 1
 | otherwise = a * a *** (b-1)

getInv :: String -> String -> [String] -- part 1
getInv a b
 | length a /= length b = getInv a (show $ nxt - 1) ++ getInv (show nxt) b
 | mod (length b) 2 == 1 = [] 
 | uncurry (<) h = getInv (dupe (f + 1)) b
 | uncurry (>) h' = getInv a $ dupe (s - 1)  
 | otherwise = map dupe [f .. s]
 where
  nxt = 10 *** length a
  h = halves a
  f = read $ fst h :: Int
  h' = halves b
  s = read $ fst h' :: Int

isInv :: String -> Bool -- part 2
isInv w = any splits divs
 where
  divs = filter (\i -> mod (length w) i == 0) [1 .. length w - 1]  
  splits i = allSame $ unfoldr (gen i) w 
  gen k s
   | length s < k = Nothing
   | otherwise = Just $ splitAt k s

dupe n = show n ++ show n

halves a = splitAt (div (length a) 2) a

pair [a,b] = (a,b)
pair _ = error "bad"

allSame [] = True
allSame (x:xs) = all (x==) xs

main = do
 txt <- readFile "src/input.txt"
 let ranges = splitOn "," $ init txt
 let pairs = map (pair . map read . splitOn "-") ranges :: [(Int,Int)]
 print $ sum $ map (sum . map read . filter isInv . map show . uncurry enumFromTo) pairs 
