applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f =  f x


class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail msg = error msg

instance Monad Maybe where
  return x =  Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing



-- do 表示法
foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- foo = do
--     x <- Just 3
--     y <- Just "!"
--     Just (show x ++ y)

-- 在 do 中 可以使用模式匹配
justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x


-- list Monad
-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat (map f xs)
--   fail _ = []
