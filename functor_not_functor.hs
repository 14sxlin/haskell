data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where -- not follow the rules of functor
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)
