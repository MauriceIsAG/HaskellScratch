module Main where

import Lib

main :: IO ()
main = someFunc

{-99 Haskell Problems-}

{-| Get the last element of a list-}
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs


{-| Get the second to last element of a list-}
myButtLast :: [a] -> a
myButtLast [x, _] = x
myButtLast (_:xs) = myButtLast xs

{-| Get the kth element of a list-}
elementAt :: [a] -> Int -> a
elementAt (x:_) 0 = x
elementAt (_:xs) k = elementAt xs (k - 1)

{-| Get the length of a list-}
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)


{-| Reverse a list-}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

{-| Checks if list is a palindrome.-}
myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome x
  | x == (reverse x) = True
  | otherwise = False

{-| Remove dupes in list-}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = [x] ++ compress (clean x xs)
  where clean _ [] = []
        clean y (x:xs)
          | y == x = clean y xs
          | otherwise = [x] ++ clean y xs

{-| Put duplicates in sublists-}
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = combine x xs ++ pack (clean x xs) 
  where
    combine _ [] = []
    combine x s = [[z | z <- x:s, z == x]]
    clean _ [] = []
    clean y (x:xs)
      | y == x = clean y xs
      | otherwise = [x] ++ clean y xs

{-| Does stuff-}
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode s = map (\(x:xs) -> (length (x:xs), x)) (pack s)


data List a = Single a | Multiple Int a
  deriving Show
{-| Similar to before-}
encodeModified :: (Eq a) => [a] -> [List a]
encodeModified s = map f (encode s)
  where f (1, x) = Single x
        f (n, x) = Multiple n x

decode :: [List a] -> [a]
decode s = foldr (++) [] (map f s)
  where f (Single x) = [x]
        f (Multiple n x) =  replicate n x

encodeDirect :: (Eq a) => [a] -> [List a]
encodeDirect [] = []
encodeDirect (x:xs) = [toList (count x (x:xs)) x] ++
                      encodeDirect (filter (x /=) xs)
  where count x s = length (filter (x==) s)
        toList 1 x = Single x
        toList n x = Multiple n x

dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = [x,x] ++ dupl xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery s n = foldr (++) [] (map (f n) (zip [1..] s))
  where f n (m, x)
          | m `mod` n == 0 = []
          | otherwise = [x]

spliter :: [a] -> Int -> [[a]]
spliter [] _ = []
spliter s n = [reverse (drop ((length s) - n) (reverse s))] ++ [drop n s]

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice s start stop = reverse (drop (((length s)) - stop) (reverse (drop (start - 1) s)))

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate s n = slice s ((f n (length s)) + 1) (length s) ++ slice s 1 (f n (length s))
  where f n m
          | n > m = f (n - m) m
          | n < 0 = f (m + n) m
          | otherwise = n

removeAt :: [a] -> Int -> (a, [a])
removeAt s n = (elementAt (slice s (n + 1) (n + 2)) 0,
                 slice s 1 n ++ slice s (n+2) (length s))

insertAt :: [a] -> a -> Int -> [a]
insertAt xs x n = slice xs 1 (n-1) ++ [x] ++ slice xs n (length xs)

range :: Int -> Int -> [Int]
range n1 n2 = [n1..n2]

listEq :: (Eq a) => [a] -> [a] -> Bool
listEq [] [] = True
listEq [] _ = False
listEq _ [] = False
listEq s1 s2 = False `notElem` (map (`elem`s1) s2 ++ map (`elem`s2) s1) 

listNeq :: (Eq a) => [a] -> [a] -> Bool
listNeq s1 s2
  | listEq s1 s2 = False
  | otherwise = True

listRemoveDupes :: (Eq a) => [[a]] -> [[a]]
listRemoveDupes [[]] = [[]]
listRemoveDupes [] = []
listRemoveDupes (x:xs) = [x] ++ listRemoveDupes (filter (listNeq x) xs)

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = [[]]
combinations n s = f n 1 s (map (\x -> [x]) s)
  where f n1 n2 s1 s2
          | n1 == n2 = s2
          | otherwise = f n1 (n2 + 1) s1 (listRemoveDupes 
                                          [x ++ [y] |
                                          x <- s2,
                                          y <- s1,
                                          y `notElem` x])


{- TODO the second combinatorics problem on the haskell website.-}

isDisjoint :: (Eq a) => [a] -> [a] -> Bool
isDisjoint [] [] = False
isDisjoint [] _ = True
isDisjoint _ [] = True
isDisjoint (x:xs) s2
  | x `elem` s2 = False
  | otherwise = isDisjoint xs s2

{-| TODO Finish this.-}
{-grouper :: (Eq a) => [Int] -> [a] -> [[[a]]]
grouper n s = g (map (`combinations`s) n)
      where f x s = filter (isDisjoint x) s
            g (x:y:s) 
              |y == [] = []
              |otherwise = map (\z -> g (f z y) (y:s)) x -}


sortOnLength :: [[a]] -> [[a]]
sortOnLength [] = []
sortOnLength (x:xs) = 
  sortOnLength [y | y <- xs, (length y) < (length x)]
  ++ [x]
  ++ sortOnLength [y | y <- xs, (length y) > (length x)]
        
sieveEratosthenes :: Int -> [Int]
sieveEratosthenes n = f n [2..n]
  where f n [] = []
        f n (x:xs)  = [x] ++ f n [y | y <- xs,
                                  y `notElem` (map (x*) [2..n])]

isPrime :: Int -> Bool
isPrime n = n `elem` (sieveEratosthenes n)

gcd' :: Int -> Int -> Int
gcd' n1 n2
  | n1 == n2 = n1
  | n1 > n2 = gcd' (n1 - n2) n2
  | otherwise = gcd' (n2 - n1) n1

isCoPrime :: Int -> Int -> Bool
isCoPrime n1 n2
  | (gcd' n1 n2) == 1 = True
  | otherwise = False

eulerTotient :: Int -> Int
eulerTotient n = length (filter id (map (isCoPrime n) [1..n]))

primeFactors :: Int -> [Int]
primeFactors n
  |isPrime n = [n]
  |otherwise = [f] ++ primeFactors (n `div` f)
     where f = fst (head (filter (\(x,y) ->
                       y == 0) (map (\x -> 
                                       (x, (n `mod` x))) 
                                (sieveEratosthenes n))))

encodePrimeFactors :: Int -> [(Int, Int)]
encodePrimeFactors = encode . primeFactors

eulerTotient' :: Int -> Int
eulerTotient' n = foldr (*) 1 
                  . map (\(x, y) -> 
                           (y-1) * (y^(x - 1))) 
                  . encodePrimeFactors $ n

primesRange :: Int -> Int -> [Int]
primesRange l u = filter (>=l) (sieveEratosthenes u)



combinationsWithDupes :: (Eq a) => Int -> [a] -> [[a]]
combinationsWithDupes 0 _ = [[]]
combinationsWithDupes _ [] = [[]]
combinationsWithDupes n s = f n 1 s (map (\x -> [x]) s)
  where f n1 n2 s1 s2
          | n1 == n2 = s2
          | otherwise = f n1 (n2 + 1) s1 [x ++ [y] |
                                           x <- s2,
                                           y <- s1,
                                           y `notElem` x]

{-| Fix empty list issue-}
goldbach :: Int -> (Int,Int)
goldbach n = snd
             . head 
             . filter (\(x, _) -> x == n)
             . map (\[x,y] -> ((x+y),(x,y)))
             . combinationsWithDupes 2  
             . sieveEratosthenes $ n

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList l u = map goldbach 
                    . dropWhile (<= l) $ [2,4 .. u]

grayC :: Int -> [String]
grayC n = combinationsWithDupes n 
          $ replicate n '1' ++ replicate n '0'

