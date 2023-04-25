

------------------------- Exercise 1

half :: Int -> Int
half x = div x 2

halves1 :: [Int] -> [Int]
halves1 [] = []
halves1 (x:xs) = half x : halves1 xs

halves2 :: [Int] -> [Int]
halves2 xs = map(half)(xs)

halves3 :: [Int] -> [Int]
halves3 ns = [ half n | n <- ns ]

-------------------------

evens1 :: [Int] -> [Int]
evens1 [] = []
evens1 (x:xs)
 | even x = x : evens1 xs
 | otherwise = evens1 xs

evens2 :: [Int] -> [Int]
evens2 xs = filter(even)(xs)

evens3 :: [Int] -> [Int]
evens3 ns = [ n | n <- ns , even n]

-------------------------

halveEvens1 :: [Int] -> [Int]
halveEvens1 [] = []
halveEvens1 (x:xs)
 | even x = half x : halves1 xs
 | otherwise = evens1 xs

halveEvens2 :: [Int] -> [Int]
halveEvens2 = undefined

halveEvens3 :: [Int] -> [Int]
halveEvens3 = undefined


------------------------- Exercise 2

shorts1 :: [String] -> [String]
shorts1 = undefined

shorts2 :: [String] -> [String]
shorts2 = undefined

squarePositives1 :: [Int] -> [Int]
squarePositives1 = undefined

squarePositives2 :: [Int] -> [Int]
squarePositives2 = undefined

oddLengthSums1 :: [[Int]] -> [Int]
oddLengthSums1 = undefined

oddLengthSums2 :: [[Int]] -> [Int]
oddLengthSums2 = undefined

------------------------- Exercise 3

everyother :: [a] -> [a]
everyother = undefined

same :: Eq a => [a] -> [a] -> [Int]
same = undefined


------------------------- Exercise 4

type Node = Int
type Map  = [(Node,Node)]

theMap :: Map
theMap = [(1,4), (1,6), (2,5), (3,5), (5,6)]

oneWay :: Node -> Map -> [Node]
oneWay = undefined

bothWays :: Node -> Map -> [Node]
bothWays = undefined


------------------------- Exercise 5

enumerate :: Int -> [String] -> String
enumerate = undefined


-------------------------

type Location  = String
type Character = String

type Party = [Character]

locations :: [Location]
locations = 
    [ "Braavos"          -- 0
    , "Castle Black"     -- 1
    , "Kings Landing"    -- 2
    , "Saltpans"         -- 3
    , "The Tree of Life" -- 4
    , "The Twins"        -- 5
    , "Winterfell"       -- 6
    ]

characters :: [Party]
characters =
    [ ["Jaqen","a sailor"]                    -- Braavos
    , ["Jon Snow"]                            -- Castle Black
    , ["Jaime","Queen Cersei"]                -- King's Landing
    , ["The captain"]                         -- Saltpans
    , ["The three-eyed raven"]                -- The Tree of Life
    , ["Walder Frey"]                         -- The Twins
    , ["Bran","Catelyn","Hodor","Sansa"]      -- Winterfell
    ]

------------------------- Exercise 6

data Game = Game Node Party [Party]

start :: Game
start =  Game 6 ["Ned Stark", "Robert Baratheon"] characters

--instance Show Game where
--    show (Game n p ps) = "You are in " ++ (locations !! n)

showGame :: Game -> String
showGame (Game n p ps) = undefined


------------------------- Exercise 7

go :: Game -> Int -> Game
go = undefined


-------------------------
