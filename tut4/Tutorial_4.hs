
data IntTree = Empty | Node Int IntTree IntTree
  --deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty = 0
height (Node i l r) = 1 + (max (height l) (height r))

find :: Int -> IntTree -> Bool
find _ Empty = False
find x (Node i l r)
  | x == i = True
  | otherwise = (find x l) || (find x r)


-------------------------

instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]

------------------------- Exercise 2

member :: Int -> IntTree -> Bool
member _ Empty = False
member x (Node i l r)
 | x < i = member x l
 | x > i = member x r
 | x == i = True

largest :: IntTree -> Int
largest Empty            = 0
largest (Node x l Empty) = x
largest (Node x l r)     = largest r

ordered :: IntTree -> Bool
ordered = undefined

deleteLargest :: IntTree -> IntTree
deleteLargest = undefined

delete :: Int -> IntTree -> IntTree
delete _ Empty = undefined
delete y (Node x l r)
    | y < x     = undefined
    | y > x     = undefined
    | isEmpty l = undefined
    | otherwise = undefined

------------------------- Exercise 3

{-
instance Show a => Show (Tree a) where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]
-}

------------------------- Exercise 4

data Dialogue = End
              | Choice

instance Show Dialogue where
  show = undefined

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

choose :: Dialogue -> Int -> Dialogue
choose = undefined

-------------------------

{-

bridgeOfDeath :: Dialogue
bridgeOfDeath = Choice s1
  [ ( s2 , Choice q1
    [ ( a1 , Choice q2
      [ ( a2 , Choice q3
        [ ( a3 , End e1) ] )
      ] )
    , ( a4 , Choice q2
      [ ( a2 , Choice q4
        [ ( a5 , End e2) ] )
      ] )
    , ( a6 , Choice q2
      [ ( a2 , Choice q3
        [ ( a9 , End e2) ] )
      ] )
    , ( a7 , Choice q2
      [ ( a2 , Choice q5
        [ ( a8 , End e3) ] )
      ] )
    ] )
  ]
   where s1  = "Stop. Who would cross the Bridge of Death must answer me these questions three, ere the other side he see."
         s2 = "Ask me the questions, bridgekeeper. I am not afraid."
         q1 = "What... is your name?"
         q2 = "What... is your quest?"
         q3 = "What... is your favourite colour?"
         q4 = "What... is the capital of Assyria?"
         q5 = "What... is the air-speed velocity of an unladen swallow?"

         a1 = "My name is Sir Lancelot of Camelot."
         a2 = "To seek the Holy Grail."
         a3 = "Blue."
         a4 = "Sir Robin of Camelot."
         a5 = "I don't know that!"
         a6 = "Sir Galahad of Camelot."
         a7 = "It is Arthur, King of the Britons."
         a8 = "What do you mean? An African or a European swallow?"
         a9 = "Blue. No-"

         e1 = "Right. Off you go."
         e2 = "[Thunk] WAAAAaaaaaauuuuggh"
         e3 = "Huh? I... I don't know that! [Thunk] WAAAAaaaaaauuuuggh"

-}