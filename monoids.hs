-- class Moniod m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = (length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a
-- lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

instance Monoid a => Monoid (Maybe a)  where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First {getFirst :: Maybe a} deriving (Eq,Ord,Read,Show)

instance Monoid (First a) where
  mempty = First Nothing
  First Nothing `mappend` x = x
  First (Just x) `mappend` _ = First (Just x)
