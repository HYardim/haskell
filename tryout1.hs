{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Redundant list comprehension" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use even" #-}
import Data.List (sort)
import Distribution.Simple.Utils (xargs)
import Language.Haskell.TH.Syntax (nothingName)

myAbs1 x
    | x < 0     = -x
    | otherwise = x
func1 xs= [abs x | x<-xs]

secondLargest xs
  | length xs < 2 = error "List must have at least two elements"
  | otherwise     = sortedList !! 1
  where sortedList = reverse (sort xs)
listIntersection :: (Foldable t, Eq a) => [a] -> t a -> [a]
listIntersection xs ys = [x | x <- xs, x `elem` ys]

triangleArea b h = 
    let area = b*h/2
    in area

sumSquare xs =
    let total = sum[x*x|x<-xs]
    in total

averageList xs = 
    let total = sum[x | x<-xs]
    in fromIntegral total / fromIntegral (length xs)

sumTuple (a,b) = a+b
safeHead [] = Nothing
safeHead (x: _) = x

describeList [] = "empty list"
describeList [x] = "has one element"
describeList _ = "has many elements"

swapPairs [] = []
swapPairs [x] = [x]
swapPairs (x:y:xs) = y:x: swapPairs xs

swapTriples [] = []
swapTriples [x] = [x]
swapTriples [x,y] = [x,y]
swapTriples(x:y:z:xs) = z:x:y: swapTriples xs

sumList [] = 0
sumList (x:xs) = x + sumList xs

myElem a [] = False
myElem a (x:xs) = a == x || myElem a xs

generateSquares n = [x*x| x<-[1..n]]

evenOnly :: [Int] -> [Int]
evenOnly xs = [x | x <- xs, x `mod` 2 == 0]

tripleMax a b c 
    | a>b && a>c = a
    | b>a && b>c = b
    | otherwise = c


removeDuplicates [] = []  
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs  
  | otherwise   = x : removeDuplicates xs  

countOccurrences n xs = length[x|x<-xs, x == n]


countOccurrences2 _ [] = 0  -- Base case: Empty list, no occurrences
countOccurrences2 n (x:xs)
  | n == x    = 1 + countOccurrences n xs  -- Count the occurrence of n in x
  | otherwise = countOccurrences n xs      -- Continue recursively





