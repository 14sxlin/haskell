import Data.Char
import Data.List

-- class Functor f where
--   fmap ::(a -> b) -> f a -> f b
--
-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

instance Functor ((->) r) where -- a -> b  ==  (->) a b
  fmap f g = (\x -> f (g x))    -- (->) r 的含义是部分应用的 2元函数, 已经有了一个了

-- instance Functor ((->) r) where
--   fmap = (.)

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
