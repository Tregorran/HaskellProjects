addition :: Int -> Int
addition x = x+5

------------------------- Exercise 1

square :: Int -> Int
square x = x^2

pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c = square(a) + square(b) == square(c) 


------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = x
    | otherwise = x * factorial(x-1)


euclid :: Int -> Int -> Int
euclid x y
    | x == y = x
    | x <  y = euclid x (y-x)
    | x >  y = euclid y (x-y)
	| x < 1 = undefined
	| y < 1 = undefined

power :: Int -> Int -> Int
power x y
 | y < 1 = 1
 | otherwise = x * power x (y-1)
--note: you will need to create your own cases,
--replacing the equals (=) sign with guards


------------------------- Exercise 3

party1 = ["Robert","Cersei","Ned","Jamie"]
party2 = ["Daenerys","Jorah","Tyrion","Grey Worm","Daario","Missandei"]

member :: [String] -> String -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

member' :: [String] -> String -> Bool
member' (x:xs) y
member' = (y == x) || member' xs y
--note: as before, remove the equals sign as well as
--      the "undefined", and replace it with your own
--      pattern-matching cases and (potentially) guards

removeOne :: [String] -> String -> [String]
removeOne = undefined

------------------------- Exercise 4

members :: [String] -> [String] -> Bool
members xs    []  = undefined
members xs (y:ys)
    | member xs y = undefined
    | otherwise   = undefined

members' :: [String] -> [String] -> Bool
members' = undefined

removeAll :: [String] -> [String] -> [String]
removeAll = undefined
