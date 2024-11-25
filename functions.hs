{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use foldr" #-}
import Distribution.Simple.Setup (trueArg)
import Distribution.Compat.Lens (_1)
import Data.Char

findLargest :: [Int] -> Int

findLargest [] = error "Empty List"
findLargest [x] = x
findLargest (x:xs) = max x (findLargest xs)

printList [] = return ()
printList (y:ys) = do
    print y
    printList ys

divisible5 xs = [x | x <- xs, x `mod` 5 == 0]

consecutiveMoreThan10 :: [Int] -> Bool
consecutiveMoreThan10 [] = False
consecutiveMoreThan10 [_] = False
consecutiveMoreThan10 (x:y:xs) = (x + y > 10) || consecutiveMoreThan10 (y:xs)


--main :: IO ()
main = do
    let list1 = [1, 2, 3, 4, 5]
    let list2 = [5, 6, 2, 9, 1, 10]
    let list3 = [4, 7, 2, 3, 8, 1]

    print ("List1: " ++ show list1 ++ " -> " ++ show (consecutiveMoreThan10 list1))
    print ("List2: " ++ show list2 ++ " -> " ++ show (consecutiveMoreThan10 list2))
    print ("List3: " ++ show list3 ++ " -> " ++ show (consecutiveMoreThan10 list3))

favNums = 2:4:7:9:[1,3,5,6,8,10]
initial = "hakan" !! 1

square x = let a = x*x in a+5

factorial 0 = 1
factorial n =
    let result = n * factorial (n-1)
    in result


list1 = [14, 23, 4, 89, 12]
list2 = [23, 45, 67, 15, 93, 72]
list3 = []

divisibleBy3 = [x | x <- list1, x `mod` 3 == 0]
search xs = length [x | x<-xs, x<50]

squared x = x*x
matrix xs ys = [x*y | x<-xs, y<-ys]

list4 = matrix list1 list2

smallerOdd100 xxs = [[x | x <- xs, odd x, x < 100] | xs <- xxs]


newList = list2: list4: list1: list3

tuple = ("Hakan", 50)

familyNames = ["Hakan", "Selin", "Hasan", "Serpil"]
indexes = [1,3..10]

addVectors (x1, y1) (x2,y2) = (x1+x2, y1+y2)

coordinates = [(3.0,5.0), (10.2,6.3), (4.4,8.2), (17.1, 19.2), (8.8,9.3)]
findPythogorean (a,b)=  sqrt (a*a+b*b)
findVectorLength xs = [findPythogorean (a,b) | (a,b)<-xs]

checkVectorLength xs = length [x | x<-xs, x>10]
evalVectorLength x
    | checkVectorLength (findVectorLength x) < 1 = "Vectors are short in length"
    | checkVectorLength (findVectorLength x) < 5 = "Vectors are average in length"
    | otherwise = "Vectors are long in length"

--result = evalVectorLength(checkVectorLength(findVectorLength coordinates))
sum' [] = 0
sum' (x:xs) = x+sum' xs

f1 radius = area
    where area = 2*pi*radius
f2 num = result
   where result = num mod 2 == 1



f3 0 = 1
f3 n = result
    where result = n * f3 (n-1)

f4 a b c
    | a>b && a>c = "a is maximum"
    | b>c && b>a = "b is maximum"
    | otherwise = "c is maximum"

f6 [] = True
f6[_] = True
f6 (x:y:xs) = x<= y&& f6 (y:xs)

f7 xs = sum [x*x | x<-xs]


f9 a b =
    let n = n-1
    in a mod n == 0 && b mod n == 0 && f9 a b

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

sumConsecutive :: Num a => [a] -> [a]
sumConsecutive [] = []
sumConsecutive [x] = []
sumConsecutive (x:y:xs) = (x + y) : sumConsecutive (y:xs)

maximumList :: Ord a => [a] -> a
maximumList [] = error "no elements"
maximumList[x] = x
maximumList(x:xs) = max x (maximumList xs)


safetailConditional xs = if null xs then [] else tail xs

safetailGuards xs
    | null xs  = []
    | otherwise = tail xs

safetailPatternMatch [] = []
safetailPatternMatch (x:xs) = xs

insert a (x:xs)
    | a>x = x: insert a xs
    | otherwise = a:x:xs


qsort [] = []
qsort (x:xs) =
    qsort [a | a <- xs, a <= x]
    ++ [x] ++
    qsort [b | b <- xs, b > x]

multThree x y z = x*y*z
multWithNine = multThree 9
multWithEighteen = multWithNine 2


applyTwice f x = f (f x)
sen = applyTwice (++ " Haha") "Hey"
listX = applyTwice (3:) [1]

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

myElem a [] = False
myElem a (x:xs)
    | x /= a = myElem a xs
    | otherwise = True

nub [] = []
nub (x:xs)
    | myElem x xs = nub xs
    | otherwise = x: nub xs

isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs)
    | x<=y = isAsc (y:xs)
    | otherwise = False

hasPath [] x y = x==y
hasPath xs x y
    | x==y = True
    | otherwise =
    let xs' = [(n,m) | (n,m)<-xs, n/=x] in
    or [hasPath xs' m y |(n,m)<-xs, n==x ]


doubleEvens xs = map (*2) (filter even xs)
squareAll xs = map (^2) xs

filterPositives xs = filter (>0) xs

toUpperCase xs = map (map toUpper) xs
filterLongWords n xs = filter (\x -> length x>n) xs

incrementAll xs = map (+1) xs

filterOdds xs = filter odd xs

doubleAll xs = map (*2) xs

filterNonEmpty xs = filter (not.null) xs

toLengths xs = map length xs

filterDivisibleBy n xs = filter (\x -> mod x n == 0) xs

removeWovels xs = map (filter (\x -> not (elem x ['a', 'e', 'i', 'u', 'o']))) xs
removeVowels xs = [ [c | c <- str, not (c `elem` "aeiouAEIOU")] | str <- xs ]

capitalizeFirst xs = map capitalize xs
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs

capitalizedFirsts xs =
    let capitalize [] = []
        capitalize (x:xs) = toUpper x: xs
    in map capitalize xs

filterPalindromes xs =
    let isPalindrome [] = True
        isPalindrome x = x == reverse x

    in filter isPalindrome xs

squareAndFilterEvens xs = filter odd (map (^2) xs)

filterAndReverse xs = map reverse (filter (not . null) xs)


filterPrimes xs =
    let isPrime n
          | n<=1 = False
          | otherwise = null [x | x<-[2..n-1], mod n x == 0]
    in filter isPrime xs

filterAndIncrement xs = map (+1) (filter (>=0) xs)
filterAndSquare xs = map (^2) (filter (<5) xs)
filterAndCapitalize xs =
    let capitalize [] = []
        capitalize (y:ys) = toUpper y: capitalize ys
    in map capitalize (filter (\x-> length x>3) xs)

filterAndReverseWords xs =
    let includesA xs = filter (\x -> any (`elem` "aA") x) xs
    in map reverse (includesA xs)

filterAndHalve xs = map (/2) (filter even xs)

filterAndFactorial xs =
    let compFactorial 0 = 1
        compFactorial n = n * compFactorial (n-1)
    in map compFactorial (filter (>=0) xs)

filterAndLength xxs =
    map (\xs->length xs) (filterEmpty xxs)
    where filterEmpty xxs = filter (not.null) xxs

filterAndAverage xxs =
    let filtNeg [] = []
        filtNeg (x:xs)
            | x<0 = filtNeg xs
            | otherwise = x: filtNeg xs
        compAvg [] = 0
        compAvg ys = fromIntegral (sum ys) / fromIntegral (length ys)
    in map (compAvg . filtNeg) xxs


filterAndSquareEvenIndices xs =
    let indexed = zip [0..] xs
        evenIndices = filter (\(i, _)->even i) indexed
        squared = map (\(_,x)->x^2) evenIndices
    in squared

filterAndConcat xxs =

    let filtDigits [] = True
        filtDigits (x:xs) = not (isDigit x) && filtDigits xs
        concatStrings [] = []
        concatStrings (xs:xxs) 
            | filtDigits xs = xs ++ concatStrings xxs
            | otherwise = concatStrings xxs
        
    in concatStrings xxs


filterAndSum xs = sum (filter (>=0) xs)
filterAndDoubleOdds xs = map (*2) (filter odd xs)









