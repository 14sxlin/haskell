data Maybe a = Nothing | Just a

-- 衍生实例 type deriving
-- data MBool = False | True deriving (Ord) -- False < True

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

-- type synonyms 同义词
-- type String = [Char]
-- type AssocList k v =[(k,v)]
-- data Either a b = Left a | Right b deriving (Eq,Ord,Read,Show)


-- import qualified Data.Map as Map
--
-- data LockerState = Taken | Free deriving (Show , Eq)
--
-- type Code = String
-- type LockerMap = Map.Map Int (LockerState, Code)
--
-- lockerLookup :: Int -> LockerMap -> Either String Code
-- lockerLookup lockerNumber map =
--   case Map.lookup lockerNumber map of
--     Nothing -> Left $ "Locker number" ++ show lockerNumber ++ " does't exists"
--     Just (state,code) -> if state/= Taken
--                           then Right code
--                           else Left $ "Locker " ++ show lockerNumber ++ " was taken"

-- lockers :: LockerMap
-- lockers = Map.fromList [
--   (100,(Taken,"ZD391")),
--   (101,(Free,"JAH3I")),
--   (103,(Free,"IQSFN")),
--   (105,(Free,"QOTSA"))
-- ]


-- recursive data struct
-- data List a = Empty | Cons a (List a) deriving (Show,Read,Eq,Ord)
-- data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Show,Read,Eq,Ord)
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show,Read,Eq,Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Eq,Read)
treeSingleton :: a -> Tree a
treeSingleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = treeSingleton x
treeInsert x (Node a left right)
  | x == a = Node a left right
  | x < a = Node a (treeInsert x left) right
  | otherwise = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node e left right)
  | x == e = True
  | x > e = treeElem x right
  | x < e = treeElem x left


-- class Eq a where         -- a 必须是具体的类型
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)  -- 这是什么鬼东西 死循环 当然不会直接调用, 由 instance 覆盖其中一个方法
--   x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red == Red = True      -- 覆盖 x == y 方法
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Green = "Green Light"
  show Yellow = "Yellow Light"


-- class (Eq a) => Num a where  -- Num 的定义

-- instance Eq (Maybe m) where -- Maybe 需要一个类型
--   Just x == Just y = x == y -- 这里有问题, 因为 m 不一定是Eq
--   Nothing == Nothing = True
--   _ == _ = False

-- instance (Eq m) => Eq (Maybe m) where -- Maybe 需要一个类型 限制 m 为 Eq
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

-- :info Maybe

-- yes-no typeclass
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno x = x /= 0

instance YesNo [a] where
  yesno [] = True
  yesno _ = False

instance YesNo Bool where
  yeano = id  -- id接受一个参数并返回这个参数

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal then yesResult else noResult

class Functor f where -- f 并不是一个具体类型 而是一个接受类型的 类型构造子 也就是 data 左边的部分 且 该部分需要一个参数
  fmap ::(a -> b) -> f a -> f b -- 接受一个(a -> b)的函数 和 f 类型的 参数 a  返回一个 f类型的结果b

-- 类型构造子可以说是一种容器, 能容纳其他的类型
-- 所以 fator 的 作用就是 接受一个 映射函数 还有一个 装有a 的容器, 返回一个 装有b 的容器
-- 之所以说 f 不是具体类型, 是因为其出现的位置 后面接了类型参数, 所以推断出它是一个  类型构造子
-- 而单独出现在  -> 左边或者右边的 参数便是具体的类型

instance Functor [] where -- 这里是 [] 而不是 [a]
  fmap = map

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Tree e left right) = Tree (f e) (fmap f left) (fmap right)

instance Functor (Either a) where -- Either 接受的是2个类型参数, 所以这里要喂一个类型参数给它
  fmap f (Right b) = Right (f b)
  fmap f (Left a) = Left a

instance Functor (Map.Map k) where
  fmap f (Map.Map k v) = Map.Map k (f v)

-- Kind
--  :k Int
-- Int :: *        -- 这种就是具体类型

-- :k Maybe
-- Maybe :: * -> *  -- 这种就是函数构造子


class Tofu t where
  tofu :: j a -> t a j
  -- 要从左往右推
  -- j 的 kind 是 * -> *
  -- t 的 kind 是 * -> * -> (* -> *) 也就是接受两个类型参数的类型构造子
-- 这个类型的kind是 * -> * -> (* -> *)
data Frank a b = Frank { frankField :: b a } deriving (Show)
-- 因为后面 出现 b a 所以 b 是 * -> *
-- a 就是 * 了
-- 所以 Frank 的类型是 * -> (* -> *) -> *
-- Frank {frankField = Just "HAHA"}

instance Tofu Frank where
  todu x = Frank x
 -- tofu (Just "abd")

 data Barry t k p = Barry { yabba :: p, dabba :: t k} -- t是 * -> * , k p 是具体类型

instance Functor (Barry a b) where
  fmap f Barry(a b x) = Barray { yabba = f x , dabba = y}
