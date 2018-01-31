len list = sum [1 | _ <- list]
[x * y | x <- [1..20], y <- [20..30] , x * y > 50]
take 10 [x * y | x <- [1..20], y <- [20..30] , x * y > 50]

--range
take 10 [2,4..)

--list comprehension
[ x * 2 | x <- [1..10]]
[ x * 2 | x <- [2..10],x * 2 >= 12]
[ x | x <- [50..100], x `mod` 7 == 3]
[x | x <- [10..20] , x/=13,x/=15,x/=19]
[x * y | x <- [10..20], y <- [10..20]]
[x * y | x <- [10..20], y <- [10..20], x * y > 20]
length` xs = sum[1 | _ <- xs]


--tuple
fst (8,10)
snd (10,20)
zip [0..10] [10..30]
zip [1..] ["apple","orange","pipe"]
