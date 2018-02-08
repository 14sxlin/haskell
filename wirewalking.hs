type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
-- landLeft n (left,right) = (left + n,right)
landLeft n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing


landRight :: Birds -> Pole -> Maybe Pole
-- landRight n (left,right) = (left, right + n)
landRight n (left,right)
  | abs (right + n -left) < 4 = Just (left,right + n)
  | otherwise = Nothing

x -: f = f x


banana :: Pole -> Maybe Pole
banana _ => Nothing

routine :: Maybe Pole
routine = case landLeft 1 (0,0) of
  Nothing -> Nothing
  Just pole1 -> case landRight 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft 1 pole3

-- return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

-- 使用 do 表示法
-- routine = do
--   start <- return (0,0)
--   first <- landLeft 2 start
--   second <- landRight 2 first
--   landLeft 1 second
