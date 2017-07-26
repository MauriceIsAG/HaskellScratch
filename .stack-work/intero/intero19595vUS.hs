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

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations = 
