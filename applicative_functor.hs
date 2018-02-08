class (Functor f) => Applicative f where
  pure :: a -> f a                    -- 将值包装 applicative functor
  (<*>) :: f (a -> b) -> f a -> f b   --
  (<$>) :: (Functor f) => (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure = Just -- pure x = Just x
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
  f <$> x = fmap f x

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)


instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList  (ZipWith (\f x -> f x) fs xs)

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- sequenceA =foldr (liftA2 (:))(pure [])
