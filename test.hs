data Tree = Node Int [Tree]
t :: Tree 

t = Node 4 [Node 5 [], Node 1 [Node 6 [], Node 2 []], Node 7 [Node 3 []]]

treefold f (Node x ts) = f x (map (treefold f) ts)

fun1 (Node _ xs) = maximum (length xs : map fun1 xs)

--fun2 = treefold f
-- where
--  f i [] = i
--  f i is = i + maximum is
  
--find largest branching factor
--fun1
fun3 = treefold f
 where
  f i [] = 0
  f i is = length is


fun2 (Node val []) = val
fun2 (Node val (x:xs)) = maximum (combine (map fun2 (x:xs)) val)

combine :: [Int] -> Int -> [Int]
combine [] a = []
combine (x:xs) a = x + a : combine xs a