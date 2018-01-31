-- :t 'a'
removeNoneUppercase :: [Char] -> [Char] -- can also using String
removeNoneUppercase st = [x | x <- st, x `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial i = product [1..i]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference` :: Double -> Double
circumference` r = 2 * pi * r
-- Bool Char Tuple

-- :t head  head :: [a] -> [a]
-- :t fst   fst :: (a,b) -> a
-- : t (==) (==) :: (Eq a) => a -> a -> Bool
-- Eq ord
-- :t (>)   (>) ::  Ord a => a -> a -> Bool
-- :t show show :: Show a => a -> String   将 Show 变成 String类型
-- :t read read :: Read a => String -> a    将 String 变成 a 类型
--  read "True"  Exception: Prelude.read: no parse
read "True" || False
read "20" + 10
read "[1,2,3,4]" ++ [5]
read "5" :: Int
read "5" :: Float
(read "5" :: Float) * 4
read "[1,2,3,4]" :: [Int]
read "(3,'a')" :: (Int,Char)

-- Enum
[LT .. GT]
succ LT
pred GT

-- Bounded
minBound :: Int -- minBound :: Bounded a => a 类型转换
maxBound :: Int
minBound :: Bool
minBound :: Char

maxBound:: (Bool,Int,Char)

-- Num
-- :t 20 Num p => p
-- (*) :: Num a => a -> a -> a

-- Integral 包含(Int,Integer)
-- Floating 包含(Float Double)
-- fromIntegral :: (Num b, Integral a) => a -> b
-- length :: Foldable t => t , a -> Int
-- length [1,2,3,4,5] + 3.2 error
fromIntegral (length [1,2,3,4,5]) + 3.2
