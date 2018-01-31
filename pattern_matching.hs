lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "SORRY ,you 're out of luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe x = "Not between 1 and 5"


factorial :: (Integral a ) => a -> a
factorial 0 = 1
factorial x = x * factorial ( x - 1 )

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Ceril"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
-- not good: addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)


first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third  (_,_,z) = z

-- let xs = [(1,3),(4,3),(2,4),(5,3)]
-- [ a + b | (a,b) <- xs ] -- add all numbers together

-- [1,2,3,4] == 1:2:3:4:[]  -- True

mhead  :: [a] -> a
mhead  [] = error"Can't call head on an empty list"
mhead  (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element :" ++ show x -- tell [x]
tell (x:y:[]) = "The list has two element" ++ show x ++ " and " ++ show y -- tell [x,y]
tell (x:y:_) = "The list is long, first two elements are : " ++ show x ++ " and " ++ show y

xsSum :: (Num a) => [a] -> a
xsSum [] = 0
xsSum (h:t) = h + xsSum(t)

xsLength :: (Num b) => [a] -> b
xsLength  [] = 0
xsLength  (_:xs) = 1 + xsLength xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] -- all to reference

-- guard
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You 're underweight"
  | bmi <= normal = "You 're normal"
  | bmi <= fat = "You 're fat"
  | otherwise = "You're a whale!"
  where bmi = weight / height ^ 2 --where
        skinny = 18.5
        normal = 25.0
        fat = 30.0
        -- (skinny,normal,fat) = (18.5,25.0,30.0)

-- myCompare :: (Ord a) => a -> a -> Ordering
-- a `myCompare` b
--   | a > b = GT
--   | a == b = EQ
--   | otherwise LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let siderArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in siderArea + 2 * topArea

-- 4 * (let a = 9 in a + 1) + 2 --42 let 表达式的返回值是 in
-- [let square x = x * x in (square 5,square 3,square 2)] -- (25,9,4)

calcBmis1 :: (RealFloat a) => [(a,a)] -> [a]
calcBmis1 xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2] -- no in , just bind name

-- let 小范围 : 范围在in中
-- where 中范围 : 范围在guard块中

-- case of
mhead1 :: [a] -> a
mhead1 xs = case xs of
  [] -> error"empty list"
  (h:_) -> h

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list."
                                               xs -> "a long list."
