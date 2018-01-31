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

myTake :: (Num n) => n -> [a] -> [a]
myTake n xs
