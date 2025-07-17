-- A function that "takes" three arguments
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- compare 100 creates a function that compares its input to 100.
-- let compareWithHundred = compare 100
-- ghci> compareWithHundred 99

-- Creates a function that divides its input by 10
-- let divideByTen = (/10)
-- ghci> divideByTen 200

-- Creates a function that checks for membership in the uppercase alphabet
-- let isUpperAlphanum = (elem ['A'..'Z'])
-- ghci> isUpperAlphanum 'C'

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice (+3) 10
applyTwice (++ " HAHA") "HEY"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipWith' (+) [1,2,3] [4,5,6]
zipWith' (++) ["foo ", "bar "] ["fighters", "hoppers"]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

zip [1,2,3] "abc"
flip' zip [1,2,3] "abc"

-- Map applies a function to every element
map (+3) [1, 5, 3, 1, 6]

-- Filter keeps only elements that satisfy a predicate
filter (>3) [1, 5, 3, 1, 6]

-- Find the largest number under 100,000 divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x mod 3829 == 0

largestDivisible

-- A lambda that adds two to its argument
map (\x -> x + 2) [1,2,3]

-- This is equivalent to map (+2) [1,2,3]

zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- sum implemented with a left fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum' [1,2,3,4,5]

-- map can be implemented with a right fold
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

map' (*2) [1,2,3]

scanl (+) 0 [3,5,2,1]
scanr (+) 0 [3,5,2,1]

-- negate . (* 3) creates a function that first multiplies by 3, then negates the result.
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
