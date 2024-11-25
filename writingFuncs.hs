{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use max" #-}
double x = x*2
isOdd x = x mod 2== 1
cube x = x*x*x
minOfTwo x y = if x <y then x else y
sumOfTwo x y = x+y
isEmpty xs = null xs

firstElement xs = head xs
lastElement xs = last xs
productList [] = 1
productList (x:xs) = productList xs

concatenateLists xs ys = xs ++ ys

fibanocci 0 = 0
fibanocci 1 = 1
fibanocci n = fibanocci(n-1) + fibanocci(n-2)

isPalindrome xs = xs == reverse xs

removeDuplicates (x:xs) = [x | x<-xs, x ]

sumList xs = foldr (\x acc -> acc+x) xs
productLists xs = foldr(\x acc-> acc*x) xs
lengthList xs = foldr(\x acc-> acc+1) xs
maximumList xs = foldr(\x acc->if x>acc then x else acc) xs
minimumList xs = foldr(\x acc->if x<acc then x else acc) xs
reverseList xs = foldl (\acc x -> x : acc) [] xs

concatLists xxs = foldr (\x acc -> x++acc) [] xxs
filterEvens xs = foldr(\x acc->if even x then x:acc else acc) [] xs


