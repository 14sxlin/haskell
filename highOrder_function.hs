multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z
-- 等价于 multiThree \x -> \y -> \z -> x * Y * z 由于柯里化
-- curry
-- 由于所有的函数都是只有一个参数,所以函数作为参数传递的时候不会有歧义
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


longChainsCount :: Int
-- longChainsCount = length (filter isLong (map chain [1..100]))
--   where isLong xs = length xs > 15
longChainsCount = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- let listOfFun = map (*) [0..]
-- (listOfFun !! 3) 5

mySum :: (Num a) => [a] -> a
mySum xs = foldl (+) 0 xs
-- lazy mySum = foldl (+) 0

myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = foldl (\acc current -> if x == current then True else acc ) False xs


myMap1 :: (a -> b) -> [a] -> [b]
myMap1 f = foldr (\x acc -> f x : acc) []
-- 也可以使用左折叠的方式实现,但是(++)效率会低很多(:),所以一般都是右折叠
-- 左折叠不能处理无限长的数据结构,但是右折叠可以
myReservsel :: [a] -> [a]
myReservsel = foldl (\acc c -> c : acc) []

myReservser :: [a] -> [a]
myReservser = foldr (\c acc -> acc ++ [c]) []

mMaximum :: (Ord a) => [a] -> a
mMaximum = foldl1 (\tmax x -> if x > tmax then x else tmax)

mFilter :: (a -> Bool) ->  [a] -> [a]
mFilter f = foldr1 (\x acc -> if f x then x:acc else acc )

mHead :: [a] -> a -- just demo 效率没有比模式匹配高
mHead = foldr1 (\x _ -> x)

mTail :: [a] -> a -- just demo 效率没有比模式匹配高
mTail = foldl1 (\_ x -> x)

-- scanl (+) 0 [1..5]
-- [0,1,3,6,10,15]   -- 返回值是acc的中间状态

-- $ 优先级最低, 可以替代(),减少()的数量, function $ expression = function (expression)


-- function compose
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- map (\x -> neg (abs x)) [1..20]
-- map (neg.abs) [1..20]

-- map (\xs -> negate (sum ( tail xs))) [[1..2],[2..5],[2..7]]
-- map (negate . sum . tail)  [[1..2],[2..5],[2..7]]

-- 遇到多个参数的函数,使用 不完全应用 来变成单参数函数
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5],[6,7,8,9,0])))
-- replicate 100 . product . map (*3) . zipWith . max [1,2,3,4,5] $ [6,7,8,9,0]

-- 构建point free style
fn x = ceiling (negate (tan (cos (max 50 x)))) -- 无法去掉两端的x
fn1 = ceiling . negate . tan . cos . max 50
