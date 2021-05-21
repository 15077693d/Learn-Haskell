module Main where
import Lib ( someFunc )
import  Data.List
inRange :: Ord a => a -> a -> a -> Bool
inRange min max x = x >= min && x <= max

-- let Fucntions

inRangeLet :: Ord a => a -> a -> a -> Bool
inRangeLet min max x =
    let lb = min <= x
        up = max >= x
    in
    lb && up

-- where Functions

inRangeWhere :: Ord a => a -> a -> a -> Bool
inRangeWhere min max x = lb && up
    where
        lb = min <= x
        up = max >= x

-- Infix functions

add :: Num a => a -> a -> a
add a b = a + b

-- if 

inRangeIf :: Ord a => a -> a -> a -> Bool
inRangeIf min max x = 
    if lb then up else False
    where
        lb = min <= x
        up = max >= x
-- print (10 `add` 20)

-- Haskell is no loops... Recursion instead

fac :: (Ord p, Num p) => p -> p
fac n = 
    if n <= 1 then
        1
    else
        n * fac (n-1)

-- Guards  
facGuards :: (Ord p, Num p) => p -> p
facGuards n
 | n <= 1 = 1
 | otherwise = n * fac (n-1)

-- Auccumulators
facAux :: (Ord t, Num t) => t -> t
facAux n = aux n 1
 where
     aux n acc 
      | n <= 1 = acc 
      | otherwise = aux (n-1) (n*acc)

-- Lists
-- import Data.List!!!! head tail length init(remove 1st) null(check null)
-- 1 type
-- [1,2,3,4,5]
-- 1:2:3:4:5:[] 
-- a -> can be anything
asc :: (Ord t, Num t) => t -> t -> [t]
asc n m
    | m < n = []
    | m == n = [m]
    | m > n = n : asc (n+1) m 


list1 :: [Double]
list1=[ x/2 | x <- [10,8,6], x>6]

list2 :: [(Integer, [Char])]
list2 = [(x,y)| x<-[1,2,3], y<-["1","g"]]
main :: IO ()
main = do
        print list2