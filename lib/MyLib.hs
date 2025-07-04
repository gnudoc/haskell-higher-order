module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------
--some higher order functions---
--take a function as a param ---
--others return one as a value--
--------------------------------

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

divByTen :: (Floating a) => a -> a
divByTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

{- e.g.
ghci> flip' zip [1,2,3,4,5] "hello"  
[('h',1),('e',2),('l',3),('l',4),('o',5)]  
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]  
[5,4,3,2,1]  
-}

--------------------------------
-- map is a versatile h.o. func-
-- often gives cleaner code ----
-- than list comp would have ---
--------------------------------
addedList = map (+3) [3,4,5,6,7]
shoutyWords = map (++ "!") ["BIFF","BANG","BOP"]
tripleDown = map (replicate 3) [3..10]
squareUp = map (map (^2)) [[1,2], [2,3,4], [3,4,5,6],[10,15,20,25,30,35]]


--------------------------------
-- filter is also v useful -----
--------------------------------

bigNums = filter (>5) [0,10,2,7,87,4,5]
eqThree = filter (==3) [3, 3.0, 7]

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smallSorted = qsort (filter (<=x) xs)
      bigSorted   = qsort (filter (>x) xs)
  in smallSorted ++ [x] ++ bigSorted

largestDiv :: (Integral a) => a
largestDiv = head (filter p [1000000, 999999..])
             where p x = x `mod` 3829 == 0

squaresum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2) -- integer division, so div not /
  | odd n  = n: chain (3*n + 1)

numLongChains :: Int -> Int -> Int
numLongChains m n= length (filter notTooShort (map chain [1..m]))
  where notTooShort xs = length xs > n
  
longestChain :: Int -> Int
longestChain m = maximum (map (length . chain) [1..m])

--------------------------------
--lambdas--when currying is not-
--enough!-----------------------

add3  = map (+3)        [1..10] -- these two are
add3' = map (\x -> x+3) [1..10] -- equivalent.

numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- lambdas start with a \ and can have as many params as needed
zipped = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

sumPairs = map (\(a,b) -> a+b) [(1,2),(3,4),(5,6),(7,8)]
