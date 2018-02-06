import qualified Data.List as L
intersperse '.' "monkey" -- "m.o.n.k.e.y"
intersperse 0 [1..6]     -- [0,0,1,0,2,0,3,0,4,0,5,0,6]
intercalate " " ["hey","there","guys"]
transpose [[1..3],[4..6],[7..9]] -- 可以用于多项式的加减
concat ["foo","bar","car"]
concatMap (replicate 4) [1..3]
