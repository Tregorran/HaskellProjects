

-------------------------
-------- PART A --------- 
-------------------------


import Data.List

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
--  deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


------------------------- Assignment 1

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
  where
    numeral' i
      | i <= 0    = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i-1))


-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2

variables :: [Var]
variables = map (:[]) ['a'..'z'] ++ [ x : show i | i <- [1..] , x <- ['a'..'z'] ]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs []     = xs 
filterVariables xs (y:ys) = filter (/=y) (filterVariables xs ys)

fresh :: [Var] -> Var
fresh = head . filterVariables variables

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = merge [x] (used n)
used (Apply  n m) = merge (used n) (used m)


------------------------- Assignment 3


rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | z == x    = Lambda z n
    | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m)


substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
substitute x n (Apply m p) = Apply (substitute x n m) (substitute x n p)

------------------------- Assignment 4

beta :: Term -> [Term]
beta (Apply (Lambda x n) m) =
  [substitute x m n] ++
  [Apply (Lambda x n') m  | n' <- beta n] ++
  [Apply (Lambda x n)  m' | m' <- beta m]
beta (Apply n m) =
  [Apply n' m  | n' <- beta n] ++
  [Apply n  m' | m' <- beta m]
beta (Lambda x n) = [Lambda x n' | n' <- beta n]
beta (Variable _) = []


normalize :: Term -> Term
normalize n
  | null ns   = n
  | otherwise = normalize (head ns) 
  where ns = beta n

run :: Term -> IO ()
run n = do
  print n
  let ns = beta n
  if null ns then
    return ()
  else
    run (head ns)

 
-------------------------

suc    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Apply (Variable "n") (Variable "f")) (Variable "x")))))
add    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Variable "f")) (Apply (Apply (Variable "n") (Variable "f")) (Variable "x"))))))
mul    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Apply (Variable "n") (Variable "f"))) (Variable "x")))))
dec    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Apply (Variable "n") (Lambda "g" (Lambda "h" (Apply (Variable "h") (Apply (Variable "g") (Variable "f")))))) (Lambda "u" (Variable "x"))) (Lambda "x" (Variable "x")))))
minus  = Lambda "n" (Lambda "m" (Apply (Apply (Variable "m") dec) (Variable "n")))

-------------------------
-------- PART B --------- 
-------------------------

------------------------- Assignment 5

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = filter (/=x) (free n)
free (Apply  n m) = merge (free n) (free m)

abstractions :: Term -> [Var] -> Term
abstractions x [] = x
abstractions x (y:ys) = Lambda y (abstractions x ys)

applications :: Term -> [Term] -> Term
applications x [] = x
applications x (y:ys) = applications' x (reverseList (y:ys))

applications' :: Term -> [Term] -> Term
applications' x [] = x
applications' x (y:ys) = Apply (applications' x ys) y

reverseList :: [Term] -> [Term]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

lift :: Term -> Term
lift x = applications (abstractions x (free x)) (varToTerm (free x))

varToTerm :: [Var] -> [Term]
varToTerm [] = []
varToTerm (x:xs) = Variable x : varToTerm xs

super :: Term -> Term
super (Variable x) = Variable x
super (Lambda x n) = lift (super' (Lambda x n))
super (Apply n m) = Apply (super n) (super m)

super' :: Term -> Term
super' (Variable x) = Variable x
super' (Lambda x n) = Lambda x (super' n)
super' (Apply n m) = Apply (super n) (super m)

------------------------- Assignment 6

data Expr =
    V Var
  | A Expr Expr

toTerm :: Expr -> Term
toTerm (V x) = Variable x
toTerm (A x n) = Apply (toTerm x) (toTerm n)

instance Show Expr where
  show = show . toTerm

type Inst = (Var,[Var],Expr)

data Prog = Prog [Inst]

instance Show Prog where
  show (Prog ls) = unlines (map showInst ks)
    where
      ks = map showParts ls
      n  = maximum (map (length . fst) ks)
      showParts (x,xs,e) = (x ++ " " ++ unwords xs , show e)
      showInst (s,t) = take n (s ++ repeat ' ') ++ " = " ++ t

names = ['$':show i | i <- [1..] ]

-------------------------

stripAbs :: Term -> ([Var],Term)
stripAbs x = (buildList x, buildTerm x)

buildList :: Term -> [Var]
buildList (Variable x) = []
buildList (Lambda x n) = x : buildList n
buildList (Apply n m) = []

buildTerm :: Term -> Term
buildTerm (Variable x) = Variable x
buildTerm (Lambda x n) = buildTerm n
buildTerm (Apply n m) = Apply n m

takeAbs :: Term -> [Term]
takeAbs (Variable x) = []
takeAbs (Lambda x n) = [Lambda x n]
takeAbs (Apply  n m) = putList (takeAbs n) (takeAbs m)

putList :: [a] -> [a] -> [a]
putList xs [] = xs
putList [] ys = ys
putList (x:xs) ys = x : putList xs ys

toExpr :: [Var] -> Term -> Expr
toExpr (y:ys) a = toExpr' (takeAbs a) (y:ys) a

toExpr' :: [Term] -> [Var] -> Term -> Expr
toExpr' [] [] a = change a
toExpr' (x:xs) [] a = change a
toExpr' [] (y:ys) a = change a
toExpr' (x:xs) (y:ys) a = toExpr' xs ys (toExpr'' x y a)

toExpr'' :: Term -> Var -> Term -> Term
toExpr'' x y (Variable n) = Variable n
toExpr'' x y (Lambda n m)
 | check x (Lambda n m) = Variable y
 | otherwise = (Lambda n (toExpr'' x y m))
toExpr'' x y (Apply n m) = Apply (toExpr'' x y n) (toExpr'' x y m)

change :: Term -> Expr
change (Variable n) = V n
change (Apply n m) = A (change n) (change m)

check :: Term -> Term -> Bool
check (Variable x) (Variable y) 
 | x == y = True
 | otherwise = False
check (Lambda x n) (Lambda y m)
 | x == y = check n m
 | otherwise = False
check (Apply h k) (Apply n m)
 | (check h n) && (check k m) = True
 | otherwise = False
check (Lambda x n) (Variable y) = False
check (Apply h k) (Variable y) = False
check (Variable y) (Lambda x n) = False
check (Variable y) (Apply h k) = False
check (Apply h k) (Lambda x n) = False
check (Lambda x n) (Apply h k) = False

toInst :: [Var] -> (Var,Term) -> (Inst,[(Var,Term)],[Var])
toInst (x:xs) (a, b) = ((a, fst(stripAbs b), toExpr (x:xs) (snd(stripAbs b))), 
 zip (countLambda (countLambda2 b) (x:xs)) (takeAbs(snd(stripAbs b))), 
 filterVariables names (countLambda (countLambda2 b) (x:xs)))

countLambda2 :: Term -> Int
countLambda2 (Variable x) = 0
countLambda2 (Lambda x n) = 1
countLambda2 (Apply  n m) = countLambda2 n + countLambda2 m

countLambda :: Int -> [Var] -> [Var]
countLambda 0 (y:ys) = []
countLambda _ [] = []
countLambda n (y:ys) = y : countLambda (n-1) ys

prog :: Term -> Prog
prog a = Prog(aux names [("$main", super a)])
 where
  aux x [] = []
  aux (x:xs) ((a, b): rest) = putList [st(toInst (x:xs) (a,b))] (aux (trd(toInst (x:xs) (a,b))) (putList rest (nd(toInst (x:xs) (a,b)))))

st ::(a, b, c) -> a
st (a,b,c) = a

nd ::(a, b, c) -> b
nd (a,b,c) = b

trd :: (a, b, c) -> c
trd (a,b,c) = c

example2 = Apply (Variable "S") (Apply (Apply example (numeral 0)) (Variable "0"))
example3 = Apply (Apply add (numeral 1)) (Apply (Apply mul (numeral 2)) (numeral 3))
example4 = Apply (Apply example3 (Variable "S")) (Variable "0")

------------------------- Assignment 7

sub :: [(Var,Expr)] -> Expr -> Expr
sub [] (V a) = V a
sub ((v,e): rest) (V a) 
 | v == a = e
 | otherwise = sub rest (V a)
sub x (A n m) = A (sub x n) (sub x m)

step :: [Inst] -> [Expr] -> IO [Expr]
step _ [] = do 
 return []
step progs ((V e): rest) = do
 if (head e) == '$' then do
  xs <- (step' (findI e progs) ((V e) : rest))
  return xs
 else do
  putStr e
  putStr " "
  return rest
step a ((A n m): rest) = do
 return (n : m : rest)

step' :: Inst -> [Expr] -> IO [Expr]
step' (v,li,m) ((V e): rest) = do
 if (length li) > (length rest) then do
  putStrLn "*** Exception: step: insufficient arguments on stack"
  return []
 else do
  return ((sub (zip li (take (length li) rest)) m) : (remaining (take (length li) rest) rest))

findI :: Var -> [Inst] -> Inst
findI v ((a,b,c): rest)
 | v == a = (a,b,c)
 | otherwise = findI v rest

remaining :: [a] -> [b] -> [b]
remaining [] a = a
remaining (x:xs) (y:ys) = remaining xs ys

supernormalize :: Term -> IO ()
supernormalize term = do
 let Prog p = prog term
 xs <- step p [V "$main"]
 loop p xs
  
loop :: [Inst] -> [Expr] -> IO ()
loop p [] = return ()
loop p ex = do
 xs <- step p ex
 loop p xs