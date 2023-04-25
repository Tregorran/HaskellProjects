

party1 = ["Catelyn","Rickard","Robb","Theon"]
party2 = ["Daenerys","Jorah","Grey Worm","Tyrion","Daario","Missandei"]


------------------------- Exercise 1

before :: [Char] -> [Char] -> Bool
before [] _ = False
before _ [] = False
before (x:xs) (y:ys)
 | x < y = True
 | x == y = before xs ys
 | otherwise = False

before' :: [Char] -> [Char] -> Bool
before' (x:xs) (y:ys) = x < y || before' xs ys

sorted :: [String] -> Bool
sorted [] = False
sorted [x] = True
sorted (x:y:xs) = before x y && sorted (y:xs)


------------------------- Exercise 2

type Node = Int
type Map  = [(Node,Node)]
type Location = String
type Party = String
type Character = Party

theMap :: Map
theMap = [(1,4), (1,6), (2,5), (3,5), (5,6)]

locations = 
    [ "Braavos"          -- 0
    , "Castle Black"     -- 1
    , "Kings Landing"    -- 2
    , "Saltpans"         -- 3
    , "The Tree of Life" -- 4
    , "The Twins"        -- 5
    , "Winterfell"       -- 6
    ]

characters =
    [ ["Jaqen","a sailor"]                    -- Braavos
    , ["Jon Snow"]                            -- Castle Black
    , ["Jaime","King Robert","Queen Cersei"]  -- King's Landing
    , ["The captain"]                         -- Saltpans
    , ["The three-eyed raven"]                -- The Tree of Life
    , ["Walder Frey"]                         -- The Twins
    , ["Bran","Catelyn","Hodor","Sansa"]      -- Winterfell
    ]

oneWay :: Node -> Map -> [Node]
oneWay n [] = []
oneWay n ((x,y):xs)
 | n == x = y : oneWay n xs
 | otherwise = oneWay n xs

bothWays :: Node -> Map -> [Node]
bothWays n [] = []
bothWays n ((x,y):xs)
 | n == y = x : bothWays n xs
 | n == x = y : bothWays n xs
 | otherwise = bothWays n xs


------------------------- Exercise 3

at :: [a] -> Int -> a
at [x:xs] 0 = x
at [x:xs] n = at xs (n-1)

--aux :: Node -> Character -> [Party] -> Node
findNode _ _ [] = error "Character not found"
findNode i c (p:ps)
  | undefined = i
  | otherwise = undefined (i+1)

--findCharacter :: Character -> Location
findCharacter c = undefined

------------------------- Exercise 4

--oneWay   :: Eq a => a -> [(a,b)] -> [b]
--bothWays :: Eq a => a -> [(a,a)] -> [a]


------------------------- Exercise 5

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = undefined
merge [] ys = undefined
merge (x:xs) (y:ys)
    | x <  y    = undefined
    | x == y    = undefined
    | otherwise = undefined

minus :: Ord a => [a] -> [a] -> [a]
minus = undefined

msort :: Ord a => [a] -> [a]
msort []  = undefined
msort [x] = undefined
msort xs  = undefined
