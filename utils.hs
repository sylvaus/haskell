{-
Install Haskell: sudo apt-get install haskell-platform
Update Cabal: sudo cabal install cabal-install --global

-}

-- Defining functions
doubleMe x = x + x 
doubleUs x y = x*2 + y*2 
doubleSmallNumber x = (if x > 100 then x else x*2) + 1  
-- Explicit declaration
additon :: Int -> Int -> Int
additon x y = x + y

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- Where and guard usage
numTellDouble :: (RealFloat a) => a -> String  
numTellDouble num  
    | fact <= 0.0 = "Negtive number"  
    | fact <= 20.0 = "Small number"  
    | fact <= 3000.0 = "Fairly big number"  
    | otherwise   = "Big number" 
    where fact = doubleMe num

-- Case usage
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

-- Let usage
square_list = [let square x = x * x in (square 5, square 3, square 2)]  

-- Map and lambda usages
map_lambda = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]

-- Function composition 
function_composition = map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 

-- Lists
-- Texas range
texas_range = [1..20]
-- Head get the first element
head_texas_range = head texas_range
-- Tail get the list without the first element
tail_texas_range = tail texas_range
-- Init get the list wihtout the last element
init_texas_range = init texas_range
-- last get the last element
last_texas_range = last texas_range
-- List comprehension with condtion
list_comprehension = [ x | x <- texas_range, x /= 13, x /= 15, x /= 19] 

-- Tuples
-- fst takes the first element of a tuple 
first_elt = fst (8,11)  
-- snd takes the second elment of a tuple
second_elt = snd (8,11)

-- main function
main = print ()  