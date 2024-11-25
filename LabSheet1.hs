{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use even" #-}
import Data.Char (toUpper, toLower)

doubleMe :: Int -> Int
doubleMe x = x + x
squareMe x = x*x
pyth x y = squareMe x + squareMe y

isTriple x y z = pyth x y == squareMe z
isTripleAny x y z = isTriple x y z || isTriple y z x || isTriple z x y

halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [x | x <- xs, x >= a, x <= b]
-- [What to return here | loop&conditionals]
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

capitalised :: String -> String 
capitalised xs = [if i == 0 then toUpper x else toLower x | (x, i) <- zip xs [0..]]

--check if the length of the string is greater than 4
checkLength:: String -> Bool
checkLength x = length x > 4 
title :: [String] -> [String] 
title xs = [capitalised x | x <- xs, checkLength x]

hello_worlds 0 = return ()
hello_worlds n = do
    putStrLn "Hello World"
    hello_worlds (n - 1)


