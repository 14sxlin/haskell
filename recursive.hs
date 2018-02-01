myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error"empty list"
myMaximum [x] = x
myMaximum (h:t)
  | h > maxTail = h
  | otherwise = maxTail
  where maxTail = myMaximum t

myReplicate :: (Num i,Ord i) => i -> a -> [a]
myReplicate n x
  | n <= 1 = []
  | otherwise = x : myReplicate (n-1) x
-- *Note*: Num 不是 Ord 的子集, 表示数字不一定得拘泥于排序,
-- 这就是在做加减法比较时要将 Num 与 Ord 类型约束区别开来的原因.

myTake :: (Num n ,Ord n) => n -> [a] -> [a]
myTake n xs
  | n <= 0 = []
myTake n (h:t) = h : myTake (n-1) t

myReserve :: [a] -> [a]
myReserve [] = []
myReserve (h:t) = myReserve t ++ [h]

myRepeat :: (Num i,Ord i) => i -> x -> [x]
myRepeat i _
  | i <=0 = []
myRepeat i x = x : myRepeat (i-1) x


myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (h1:t1) (h2:t2) = [(h1,h2)] ++ myZip t1 t2

melem :: (Eq a) => a -> [a] -> Bool
melem a [] = False
melem a (h:t)
  | a==h  = True
  | otherwise = melem a t

quickSort :: (Ord n) => [n]  -> [n]
quickSort [] = []
quickSort (h:t) = quickSort smaller ++ [h] ++ quickSort bigger
  where smaller = [x | x <- t, x <= h ]
        bigger = [x | x <- t, x > h]
