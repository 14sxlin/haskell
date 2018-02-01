multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z
-- curry
-- let multiTwoWith9 = multiThree 9
-- 函数不是 Show 类型类的实例 所以不能打印

compareWith100 :: (Num x, Ord x) => x -> Ordering
compareWith100 x = compare 100 x

-- 中缀表达式的 不完全调用
divideByTen :: (Floating x) => x -> x
divideByTen = (/10) -- divideByTen 200 = (/10) 200 = 200/10

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- applyTwice (+3) 10  = 16
-- applyTwice (3:) [1] = [3,3,1]

myZipWith :: (a -> b -> c) -> [a] -> [b] ->[c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x
-- myFlip zip [1,2,3,4,5,6] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- non exhaustive? why
myFilter :: (a -> Bool) -> [a] ->[a]
myFilter _ [] = []
myFliter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort (filter (<=x) xs)
      biggerSorted = quickSort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted


-- Collatz
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)
