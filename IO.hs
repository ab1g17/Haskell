import System.IO
import Data.Char 
import Data.List 
import Prelude
import qualified Data.Map as Map
import Data.Char as Char
import Data.List (genericSplitAt) 
import qualified Data.Text as T
import Data.Ord
import Data.Maybe


data Person =
  Adult
    { adultFirstName :: String
    , adultLastName :: String
    , adultEmail :: String
    , adultAge :: Int
    , adultOccupation :: Int
    } |
  Child
    { childFirstName :: String
    , childAge :: Int
    , childGrade :: Int
    }

lastButOne :: (Eq a) => [a] -> Maybe a
lastButOne [] = Nothing
lastButOne [x] = Nothing
lastButOne (x:y:[]) = Just x
lastButOne (x:y:xs) = lastButOne (y:xs)
--lastButOne (x:y:xs) 

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   -- (x:xs) means basically loop over list (remembering the head)
    | x > maxTail = x  
    | otherwise = maxTail  
        where maxTail = maximum' xs 

ree [] = []
ree (x : xs) = ree xs ++ [x]

lengthh :: (Num b) => [a] -> b
lengthh [] = 0
lengthh [x] = 1
lengthh (x:xs) = 1 + lengthh xs

--summ :: (Show a) => [a] -> a
summ [] = 0
summ [x] = x
summ xs = 
  let total = sum xs
      elements = length xs
      elementsInt = fromIntegral elements
  in total / elementsInt


myMean [] = 0.0
myMean xs = (mySum xs) / (fromIntegral (length xs))
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

reverser [] = []
reverser (x:xs) = 
    let head = x
        reversedTail = reverse xs ++ [head]
    in (x:xs) ++ reversedTail

palin [] = False
palin [x] = True
palin (x:y:[]) = if x == y then True else False
palin (x:xs) = 
  let rev = reverse (x:xs)
  in (x:xs) == rev

{-}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

findElem :: (Ord a) => a -> Tree a -> Bool
findElem x EmptyTree = False
findElem x (Node a left right)
    | x == a = True
    | x < a = findElem x left
    | x > a = findElem x right
   
lol = Node 2 EmptyTree EmptyTree

addElem :: (Ord a) => a -> Tree a -> Tree a
addElem x EmptyTree = Node x EmptyTree EmptyTree
addElem x (Node a left right)
    | x == a = Node a left right
    | x < a = Node a (addElem x left) right
    | x > a = Node a left (addElem x right)

leafCount :: (Num b) => Tree a -> b
leafCount EmptyTree = 0
leafCount (Node _ EmptyTree EmptyTree) = 1
leaftCount (Node _ l r) = leafCount l + leafCount r
 -}
myLast :: [a] -> a
myLast [] = error "Nein"
myLast [x] = x
myLast (_:xs) = myLast xs

lastOne :: [a] -> a
lastOne [] = error "Nein"
lastOne [x] = error "Neiner"
lastOne (x:y:[]) = x
lastOne (x:xs) = lastOne xs

--kth :: (Num a,Show b) => a -> [b] -> b
kth _ [] = error "Nein"
kth 1 (x:xs) = x
kth n (x:xs) = kth (n - 1) xs

rev :: [a] -> [a]
rev [] = error "Empty list boi"
rev [x] = [x]
rev (x:xs) = rev xs ++ [x]

dup :: (Eq a) => [a] -> [a]
dup [] = []
dup [x] = [x]
dup (x:xs) 
  | x == head xs = dup xs
  | otherwise = (x:dup xs)


dups :: (Eq a) => [a] -> [a]
dups [] = []
dups [x] = [x]
dups (x:xs) 
    | x == head xs = dups xs 
    | otherwise = (x:dups xs)

dupli :: [a] -> [a]
dupli [] = []
dupli [x] = (x:x:[])
dupli (x:xs) = (x:x:dupli xs)

--split :: Bounded b => b -> [a] -> [a]
splitt :: Int -> [a] -> ([a],[a])
splitt n xs = (take n xs , drop n xs)
  
slice :: Int -> Int -> [a] -> [a]
slice n m xs = listTwo
    where listOne = drop (n - 1) xs
          listTwo = take (m - n + 1) listOne
          
          
removeAt :: Int -> [a] -> [a]
removeAt 0 xs = xs
removeAt 1 (_:xs) = xs
removeAt n (x:xs) = [x] ++ removeAt (n - 1) xs

insertAt :: (Show b) => b -> Int -> [b] -> [b]
insertAt n _ [] = [n]
insertAt n 1 xs = (n:xs)
insertAt n m (x:xs) = [x] ++ insertAt n (m-1) xs

range :: Int -> Int -> [Int]
range n m 
  | n > m = []
  | n == m = [m]
  | n < m = (n:n+1:[]) ++ range (n + 2) m



data BookInfo = Book Int String deriving (Show) 
myBook = Book 2 "S"

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime 3 = True
prime n = if 0 `elem` (map (n `mod`) [2..n-1]) then False  else True
              
gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m = gcd' m (n `mod` m)

coGcd :: Int -> Int -> Bool
coGcd n m = if gcd n m == 1 then True else False

primeRange :: Int -> Int -> [Int]
primeRange n m 
  | n > m = error "Invalid range"
  | otherwise = [ a | a <- [n+1..m-1], prime a ]

leapYear :: Int -> Bool
leapYear n 
  | p1 && p2 || p1 && p3 = True
  | otherwise = False
  where p1 = n `mod` 4 == 0
        p2 = n `mod` 100 /= 0
        p3 = n `mod` 100 /= 0 && n `mod` 400 == 0
      
data Planet = Mercury  
               | Venus 
               | Earth  
               | Mars 
               | Jupiter 
               | Saturn 
               | Uranus 
               | Neptune deriving (Eq, Show)

getConstant :: Planet -> Float
getConstant p 
  | p == Mercury = 0.2408467

ageOn :: Planet -> Float -> Float
ageOn planet age = (getConstant planet) * age

pangram :: [Char] -> Bool
pangram [] = error "Empty String"
pangram input = if False `elem` test then False else True
  where test = [if a `elem` input then True else False | a <- ['a'..'z'] ]

collatz :: Int -> Int
collatz 1 = 0
collatz n 
  | n `mod` 2 == 0 = 1 + collatz (n `div` 2) 
  | otherwise = 1 + collatz (n * 3 + 1)   
  
transc :: [Char] -> [Char]
transc input = [ if a == 'A' then 'U' else if a == 'T' then 'A' else if a == 'C' then 'G' else 'C' | a <- input]

--countNuc :: [Char] -> ((a),(b))
count n input = (length (filter (==n) input):[])

--countNuc :: [Char] -> [a]


grouper :: (Ord a) => [a] -> [[a]]
grouper input = group (sort input)

lengther :: Foldable t => [t a] -> [Int]
lengther [] = []
lengther (x:xs) = (length x:lengther xs)

--function that counts occurences of nucleotides from string input
countNuc :: Ord a => [a] -> [(Char, Int)]
countNuc input = zip ['A','C','G','T'] (lengther (grouper input))

--check if number is armstrong number
armstrong :: (Integral a, Show a) => a -> Bool
armstrong x =
    x == sum (map raiseDigit [0..(numDigits-1)])
    where
        numDigits = length $ show x
        nthDigit n = mod (div x (10^n)) 10
        raiseDigit n = (nthDigit n)^numDigits


digits n = map (\x -> read [x] :: Int) (show n)

--armstrongg :: (Integral a, Show a) => a -> Bool
armstrongg x =
    x == sum (map (^numDigits) digitss)
    where
        numDigits = length $ show x
        digitss = digits x
        --raiseDigit n = (nthDigit n)^numDigits

squareSum :: Integer -> Integer
squareSum n = result
  where 
     sumOfSquares = sum[ a^2 | a <- [1..n] ]
     squareOfSum =  (sum[1..n])^2 
     result = squareOfSum - sumOfSquares

acronym :: [Char] -> [Char]
acronym input = firstLetters
  where
    wordList = words input
    firstLetters = [ toUpper (a !! 0) | a <- wordList ] 

aliquot :: Integer -> [Char]
aliquot n 
  | factorSum == n = "Perfect Number"
  | factorSum > n = "Abundant Number"
  | otherwise = "Deficient Number"
  where
    factors = [ a | a <- [1..n-1] , n `mod` a == 0]
    factorSum = sum factors

hamming :: [Char] -> [Char] -> Integer   
hamming [] [] = 0
hamming xs [] = error "Unequal strands"
hamming [] ys = error "Unequal strands"
hamming [x] [y] = if x /= y then 1 else 0
hamming (x:xs) (y:ys) 
  | x /= y = 1 + hamming xs ys 
  | otherwise = hamming xs ys  



  
type School = Map.Map Int [String]
--add a student in a given grade in the school (map)
-- I think instead of <> , ++ would also work
add :: Int -> String -> School -> School
add gradeNum student = Map.insertWith (<>) gradeNum [student]

empty :: School
empty = Map.empty

--return all the students in a grade
grade :: Int -> School -> [String]
grade g s = sort $ Map.findWithDefault [] g s

--return the entire roster sorted by grade
sorted :: School -> [(Int, [String])]
sorted school = (fmap . fmap) sort s
    where
        s = Map.toAscList school

isogram :: [Char] -> Bool
isogram input 
  | '-' `elem` input = hyphenRemoval input == nub (hyphenRemoval input)
  | otherwise = nub input == input
  where
    hyphenRemoval x = filter (/='-') x

type OldSystem = Map.Map Int [Char]
type NewSystem = Map.Map [Char] Int

--setUpOldSystem :: oldSystem
setUpOldSystem  = old
  where old = Map.fromList [(1, ['A','E','I','O','U','L','N','R','T']),
                            (2, ['D','G']),
                            (3, ['B','C','M','P']),
                            (4, ['F','F','V','W','Y']),
                            (5, ['K']),
                            (8, ['J','X']),
                            (10, ['Q','Z'])
                           ] 

data Color =  Black 
                  | Brown 
                  | Red 
                  | Orange
                  | Yellow 
                  | Green 
                  | Blue 
                  | Violet 
                  | Grey 
                  | White deriving Eq

colorToNum :: Color -> Int
colorToNum color 
  | color == Black = 0
  | color == Brown = 1
  | color == Red = 2
  | color == Orange = 3
  | color == Yellow = 4
  | color == Green = 5
  | color == Blue = 6
  | color == Violet = 7
  | color == Grey = 8
  | color == White = 9
  | otherwise = error "Invalid input"

value :: (Color,Color) -> [Char]
value (c1,c2) = show (colorToNum c1) ++ show (colorToNum c2)
  
valueTrio :: (Color,Color,Color) -> [Char]
valueTrio (c1,c2,c3) = show (read (value (c1,c2)) * 10^(colorToNum c3)) ++ " ohms"

--basically map function
accumulate :: (a -> b) -> [a] -> [b]
accumulate input [] = []
accumulate input [x] = [input $ x] 
accumulate input (x:xs) = [input $ x] ++ accumulate input xs 


discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p [x] = if (p $ x) then [] else [x]
discard p (x:xs) 
  | p $ x = discard p xs
  | otherwise = [x] ++ discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep  p [x] = if (p $ x) then [x] else []
keep  p (x:xs) 
  | p $ x = [x] ++ keep  p xs
  | otherwise = keep  p xs

nanp :: [Char] -> [Char]
nanp [] = []
nanp [x] = [x]
nanp (x:xs) 
  | x == '+' && head xs == '1' = filter (Char.isDigit)  (tail xs)
  | x == '+' && head xs /= '1' = error "Invalid area code"
  | otherwise = filter (Char.isDigit) (x:xs)

--check for valid isbn number 
isbn :: String -> Bool
isbn xs = lengthCheck && (sumCheck $ sum $ lastDigit : isbn' digits 10)
    where
      lengthCheck = 10 == (length $ filter isAlphaNum xs)
      digits      = map digitToInt $ filter isDigit xs
      lastDigit   = if last xs == 'X' then 10 else 0
      sumCheck s  = s `rem` 11 == 0
  
isbn' :: [Int] -> Int -> [Int]
isbn' [] _ = []
isbn' xs p = (head xs) * p : isbn' (tail xs) (p - 1)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht
              deriving Eq

yacht :: Category -> [Int] -> Int
yacht category dice 
  | category == Ones = length (1 `elemIndices` dice)
  | category == Twos = (length (2 `elemIndices` dice)) * 2
  | category == Threes = (length (3 `elemIndices` dice)) * 3
  | category == Fours = (length (4 `elemIndices` dice)) * 4
  | category == Fives = (length (5 `elemIndices` dice)) * 5
  | category == Sixes = (length (6 `elemIndices` dice)) * 6
  | category == FullHouse = fullHouseHelper dice
  | category == FourOfAKind = fourOfAKindHelper dice
  | category == LittleStraight = littleStraightHelper dice
  | category == BigStraight = bigStraightHelper dice
  | category == Choice = sum dice
  | category == Yacht = yachtHelper dice
            
fullHouseHelper dice = score 
  where grouper = group dice
        checker = length grouper == 2
        score = if checker then sum (concat grouper) else 0

fourOfAKindHelper dice = score
  where grouper = group dice
        checker n = if length (grouper !! n) == 4 then grouper !! n else [0]
        score = if checker 0 /= [0] then sum (checker 0) else if checker 1 /= [0] then sum (checker 1) else 0

littleStraightHelper dice 
  | dice == [1,2,3,4,5] = 30
  | otherwise = 0

bigStraightHelper dice  
  | dice == [2,3,4,5,6] = 30
  | otherwise = 0

yachtHelper dice
  | length (group dice) == 1 = 50
  | otherwise = 50

--decode :: String -> String
encode encodedText = (concat (putTogether lengthList (nubGrouper grouper)))
  where grouper = groupBy (==) encodedText
        lengthList = helper grouper
        nubGrouper [x] = (nub x:[])
        nubGrouper (x:xs) = (nub x : nubGrouper xs)
        
helper [x] = (show(length [x]):[]) 
helper (x:xs)  = (show(length x) : helper xs)
  
--putTogether :: [Int] -> [Char] -> [Char]
putTogether [x] [y] = (x : y  : [])
putTogether (x:xs) (y:ys) = (show x : y : putTogether xs ys )

--better encode
encodee :: String -> String
encodee text = concat( map f (group text))

f [] = []
f [x] = [x]
f (x:xs) = show ((length xs) + 1) ++ [x]



splitBy delimiter = foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l
                   | otherwise = (c:x):xs

-- input "12WB12W3B24WB" for tests
decode :: String -> String
decode [] = []
decode (x:y:z:xs)
    |isNumber x && isNumber y = replicate ((digitToInt y) + ((digitToInt x) * 10)) z ++ decode xs
    |isNumber x = replicate (digitToInt x) y ++ decode (z:xs)
    |otherwise = x:decode (y:z:xs)
decode (x:y:xs)
    |isNumber x = replicate (digitToInt x) y ++ decode (xs)
    |otherwise = x:decode (y:xs)
decode (x:xs) = (x:xs)
                       
data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c 
  | (a <= 0 || b <= 0 || c <= 0) || (a + b < c || a + c < b || b + c < a) = Illegal
  | a == b && a == c = Equilateral
  | a == b || a == c || b == c = Isosceles
  | otherwise = Scalene

scrabbleScore :: String -> Int
scrabbleScore word = sum (fun word)

fun [] = []
fun [x] = score x : []
fun (x:xs) = score x : fun xs
  
score x 
  | x `elem` ['A','E','I','O','U','L','N','R','S','T'] = 1 
  | x `elem` ['D','G'] = 2 
  | x `elem` ['B','M','C','P'] = 3 
  | x `elem` [ 'F','H','V','W','Y'] = 4 
  | x == 'K'= 5 
  | x `elem` ['J','X'] = 8 
  | x `elem` ['Q','Z'] = 10 
  | otherwise = 0


data Direction = North | South | West | East deriving (Show, Eq)

data Robot =
  Robot
    {
      coordinates :: Coordinates,
      direction :: Direction
    } 
    deriving (Show,Eq)

type Coordinates = (Int,Int)

mkRobot :: Coordinates -> Direction -> Robot
mkRobot coord dir = Robot coord dir

testRobo = mkRobot (0,0) West

move :: Robot -> String -> Robot
move robot [x] = moveHelper robot x
move robot (x:instructions) = move (moveHelper robot x) instructions
 
moveHelper robot x
  | x == 'R' && direction robot == West =  mkRobot (coordinates robot) North
  | x == 'R' && direction robot == East = mkRobot (coordinates robot) South
  | x == 'R' && direction robot == South = mkRobot (coordinates robot) West
  | x == 'R' && direction robot == North = mkRobot (coordinates robot) East 
  | x == 'L' && direction robot == West = mkRobot (coordinates robot) South
  | x == 'L' && direction robot == East = mkRobot (coordinates robot) North
  | x == 'L' && direction robot == South = mkRobot (coordinates robot) East 
  | x == 'L' && direction robot == North = mkRobot (coordinates robot) West
  | x == 'A' && direction robot == West = mkRobot ((fst (coordinates robot) - 1), snd (coordinates robot)) West
  | x == 'A' && direction robot == East = mkRobot ((fst (coordinates robot) + 1), snd (coordinates robot)) East
  | x == 'A' && direction robot == South = mkRobot (fst (coordinates robot), (snd (coordinates robot)) - 1) South
  | x == 'A' && direction robot == North = mkRobot (fst (coordinates robot), (snd (coordinates robot)) + 1) North
  | otherwise = robot


data Protein = Methionine 
              | Phenylalanine 
              | Leucine
              | Serine
              | Tyrosine
              | Cysteine
              | Tryptophan
              | STOP
              deriving (Show, Eq)
            


protein :: [[Char]] -> [Protein]
protein [x] = proteinHelper x : []
protein (x:xs) = nub (filter (/=STOP) (proteinHelper x : protein xs))
  

proteinHelper x 
  | x == "AUG" = Methionine 
  | x == "UUU" || x == "UUC" = Phenylalanine 
  | x == "UUA" || x == "UUG" = Leucine 
  | x == "UCU" || x == "UCC" || x == "UCA" || x == "UCG" = Serine 
  | x == "UAU" || x == "UAC" = Tyrosine 
  | x == "UGU" || x == "UGC" = Cysteine 
  | x == "UGG" = Tryptophan 
  | otherwise = STOP

codons input = splitBy ('.') (changeInput input 3)

changeInput [x] count = [x]
changeInput (x:xs) count 
  | count == 1 = (x :'.': changeInput xs count)
  | otherwise = (x : changeInput xs (count -1))
  


lolo xs = changeInput xs

data Character =
   Character
    {
      hitpoints :: Int,
      strength :: Int,
      dexterity :: Int,
      constitution :: Int,
      intelligence :: Int,
      wisdom :: Int,
      charisma :: Int
    }
    deriving (Show,Eq)
 

createCh hp st dex con int wis char = Character hp st dex con int wis char

makeCharacter :: Character
makeCharacter = start dice1 dice2 dice3 dice4 dice5 dice6
  where dice1 = [5, 3, 1, 6] 
        dice2 =  [3, 2, 5, 3]
        dice3 = [1, 1, 1, 1]
        dice4 = [2, 1, 6, 6]
        dice5 = [3, 5, 3, 4]
        dice6 = [6, 6, 6, 6]

start d1 d2 d3 d4 d5 d6 = createCh (calcHP(sum(discarder d3))) (sum(discarder d1)) (sum(discarder d2)) (sum(discarder d3)) (sum(discarder d4)) (sum(discarder d5)) (sum(discarder d6)) 


calcHP input = 10 + floor ((fromIntegral(input) - 10) / 2)
discarder input = take 3 (sortBy (flip compare) input)

anagram word [x] = helperGram word x : []
anagram word (x:xs) = (helperGram word x : anagram word xs)

helperGram :: String -> String -> String
helperGram word wordToCheck 
  | sort wordToCheck == sort word = wordToCheck
  | otherwise = []
  

data LinkedList a = EmptyList | ListNode a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum EmptyList = error "Empty List"
datum (ListNode a _ ) = a
  

fromList :: [a] -> LinkedList a 
fromList [x] = ListNode x EmptyList
fromList (x:xs) = ListNode x (fromList xs)

isNil :: LinkedList a -> Bool
isNil EmptyList = True
isNil (ListNode _ _)  = False

new :: a -> LinkedList a -> LinkedList a
new x EmptyList = ListNode x (EmptyList)
new x linkedList = ListNode x (linkedList) 

next :: LinkedList a -> LinkedList a
next EmptyList = EmptyList
next (ListNode _ a) = a

nil :: LinkedList a
nil = EmptyList

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = fromList (reverse (toList linkedList))

toList :: LinkedList a -> [a]
toList EmptyList = []
toList (ListNode a b) = a : toList b  

concater [] = []
concater (x : xs) = x ++ concater xs 

revBoi [] = []
revBoi [x] = [x]
revBoi (x:xs) = (revBoi xs) ++ [x]

filterBoi p [x] = if p x then [x] else []
filterBoi p (x:xs) 
  | p x = x : (filterBoi p xs)
  | otherwise = filterBoi p xs

primeFac :: Int -> [Int]
primeFac n = findFactors n
  where findFactors n = [ a | a <- [2..n-1] , n `mod` a == 0, prime a ]

data Value = Value Int

--Given a string of digits, calculate the largest product for a contiguous substring of digits of length size.
lsp :: Int -> [Char] -> Int
lsp size digits =  maximum (multiplier (splitBy '.' (changeInput (helperer size digits) size)))
  where 
        -- intersperse a '.' every 'size' digits so they can be split with "splitBy"
        changeInput [x] count = [x]
        changeInput (x:xs) count 
          | count == 1 = (x :'.': changeInput xs size)
          | otherwise = (x : changeInput xs (count -1))

-- get all combinations of contiguos substring of digits of size n
helperer n [x] = x : []
helperer n (x:y:[]) =  (x : y :[])
helperer n (x:xs) = (fst (splitAt n (x:xs))) ++ helperer n (head xs:tail xs)

turnToNum [x] = (read x :: Int) : []
turnToNum (x:xs) = (read x::Int) : turnToNum xs

--get the products of the combinations extracted using above functions
multiplier [x] = [product [ read [x] :: Int | x <- x ]]
multiplier (x:xs) = [product [ read [x] :: Int | x <- x ]] ++ multiplier xs 

data Clock = 
      Clock 
      {
        hour :: Int,
        minute :: Int
      }
  deriving (Eq,Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min 
  | min == 60 = error "Max minutes is 59"
  |otherwise = Clock hour min

toString :: Clock -> String
toString clock 
  | hour clock > 23 = "Invalid Time"
  | length (show (hour clock)) == 1 && length (show (minute clock)) > 1 = "0" ++ show (hour clock) ++ ":" ++ show (minute clock)
  | length (show (minute clock)) == 1 && length (show (hour clock)) > 1 = show (hour clock) ++ ":0" ++ show (minute clock)
  | length (show (minute clock)) == 1 && length (show (hour clock)) == 1 = "0" ++ show (hour clock) ++ ":0" ++ show (minute clock)
  | otherwise = show (hour clock) ++ ":" ++ show (minute clock)
 
-- if minutes are over 60 correct to the right hour and remaining minutes
correctMinutes :: Clock -> String
correctMinutes clock  
  | minute clock > 60 = toString (Clock (hour clock + ((minute clock) `div` 60)) (minute clock `mod` 60))
  | otherwise = toString clock
  

addDelta :: Int -> Int -> Clock -> String
addDelta h m clock = correctMinutes (Clock (hour clock + h) (minute clock + m))
  

pythTriplet n = [ (a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a**2 + b**2 == c**2, a < b , b < c, a + b + c == n]

series size digits =   splitBy '.' (changeInput (helperer size digits) size)
  where 
   
    changeInput [x] count = [x]
    changeInput (x:xs) count 
      | count == 1 = (x :'.': changeInput xs size)
      | otherwise = (x : changeInput xs (count -1))

--sieve of eratosthenes algorithm for getting primes from a range
sieve [x] = x : []
sieve (x:xs) = x : sieve (removeMultiples x xs)

removeMultiples x list = remove x list
  where
    remove n [x] = if x `mod` n == 0 then [] else [x]
    remove n (x:xs)
      | n /= x && x `mod` n == 0 = remove n xs
      | otherwise = x : remove n xs

finderr element sortedList
  | element == (sortedList !! ((length sortedList) `div` 2)) = element 
  | element < (sortedList !! ((length sortedList) `div` 2)) = finderr element (takeWhile (<=element) sortedList)

      
binarySearch element list = finder element sorter
  where sorter = sort (nub list)
        finder element sortedList
          | element == (sortedList !! ((length sortedList) `div` 2)) = show (Just element) ++ " " ++  show (element `elemIndex` list)
          | element < (sortedList !! ((length sortedList) `div` 2)) = finder element (takeWhile (<=element) sortedList)
          | element > (sortedList !! ((length sortedList) `div` 2)) = finder element (dropWhile (<=element) sortedList)
          | length (sortedList) == 1 && (element `elem` sortedList) == False = "Nothing"
          | length (sortedList) == 1 && element `elem` sortedList = show (Just element)
 
alphaAsc = ['a'..'z']
alphaDesc = reverse alphaAsc

--atbash cipher
atbashEncode string = helper (filter (/= ' ') string)
  where 
    helper [x] = alphaDesc !! (fromJust(x `elemIndex` alphaAsc)) : []
    helper (x:xs) = alphaDesc !! (fromJust(x `elemIndex` alphaAsc)) : helper xs
 
atbashDecode string = helper (filter (/= ' ') string)
  where 
    helper [x] = alphaAsc !! (fromJust(x `elemIndex` alphaDesc)) : []
    helper (x:xs) = alphaAsc !! (fromJust(x `elemIndex` alphaDesc)) : helper xs

data BST a = EmptyTree | Node a (BST a) (BST a) deriving (Show,Eq)

bstLeft :: BST a -> Maybe (BST a)
bstLeft EmptyTree = Nothing
bstLeft (Node a left _) =  Just left
  
bstRight :: BST a -> Maybe (BST a)
bstRight EmptyTree = Nothing
bstRight (Node a _ right) = Just right
      
bstValue :: BST a -> Maybe a
bstValue EmptyTree = Nothing
bstValue (Node b _ _) = Just b

emptyBST :: BST a
emptyBST = EmptyTree

singleton :: a -> BST a
singleton value = Node value EmptyTree EmptyTree

insertBST ::(Ord a) => a -> BST a -> BST a
insertBST value EmptyTree = singleton value
insertBST value (Node a left right) 
  | value < a = Node a (insertBST value left) right
  | value > a = Node a left (insertBST value right)
  | value == a = (Node a left right)

fromListBST list = fromListHelper (reverse list)

fromListHelper ::(Ord a) => [a] -> BST a
fromListHelper [] = EmptyTree
fromListHelper [x] = singleton x
fromListHelper (x:xs) = insertBST x (fromListHelper xs)

fromListt :: Ord a => [a] -> BST a
fromListt xs = foldr insertBST emptyBST (reverse xs)

toListBST :: BST a -> [a]
toListBST EmptyTree          = []
toListBST (Node a left right) = [a] ++ toListBST left ++ toListBST right

infList = [1..]

nthPrime :: Int -> Int
nthPrime n = [ a | a <- infList, prime a ] !! (n - 1)

wordCount :: Foldable t => t Char -> [([Char], Int)]
wordCount input =  nubber `zip` (counter sortedGroupedInput)
  where splitter = splitBy ' ' input
        sortedGroupedInput = group (sort splitter)
        nubber = nub (sort splitter)
        counter [x] = [length x]
        counter (x:xs) = [length x] ++ counter xs 

data BankAccount = 
      BankAccount 
      {
        accHolder :: String,
        balance :: Integer
      }
      deriving (Show,Eq)

openAccount :: String -> Integer -> BankAccount     
openAccount name balance = BankAccount name balance

getBalance :: BankAccount -> Integer
getBalance account = balance account

updateBalance :: BankAccount -> Integer  -> BankAccount
updateBalance account amount 
  | amount < 0 && negate amount > balance account = error "Can't withdraw more than balance"
  | otherwise = openAccount (accHolder account) ((balance account) + amount)

subList ::(Eq a) => [a] -> [a] -> Maybe Ordering
subList listA listB   
  | listA `intersect` listB == listA && length listA == length listB = Just EQ
  | listA \\ listB == [] && length listA < length listB = Just LT
  | listB \\ listA == [] && length listA > length listB = Just GT
  | otherwise = Nothing


firstN 0 = 0
firstN 1 = 1
firstN x = sum[a**2 | a <- [1..x]]

--interesting to note the type difference between these 2 
adder (x,y) = x + y
adder' x y = x + y

product' :: Num a => [a] -> a
product' [] = 0
product' [x] = x
product' (x:xs) = x * product xs

--reversed quicksort
quicktros :: Ord a => [a] -> [a]
quicktros [] = []
quicktros (x:xs) = quicksort rs ++ [x] ++ quicksort ls
                   where 
                     ls = [ a | a <- xs , a <= x ]
                     rs = [ a | a <- xs , a > x ]


quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
                   where 
                     ls = [ a | a <- xs , a <= x ]
                     rs = [ a | a <- xs , a > x ]

fourth [] = error "Empty List"
fourth (x:xs) 
    | length (x:xs) >= 4 = (!!) (x:xs) 3 
    | length (x:xs) == 4 = head (reverse (tail (x:xs)))
    | otherwise = error "List too short"


safetail xs = if null xs then [] else drop 1 xs 

safetailg xs 
    | null xs = []
    | otherwise = drop 1 xs

safetailp [] = []
safetailp [x] = [x]
safetailp (x:xs) = xs 


halve (x:xs) 
    | length (x:xs) `mod` 2 == 0 = splitAt ((length (x:xs)) `div` 2) (x:xs)
    | otherwise = error "Non even length"

luhn :: Integral a => [a] -> Bool
luhn [] = error "No Number"
luhn input = puttogether input

luhnReverser :: [a] -> [a]
luhnReverser input = reverse input

luhnSkipper :: Eq a => [a] -> Int -> [a]
luhnSkipper [] _ = []
luhnSkipper (x:xs) count
    | (fromJust (elemIndex x (x:xs)) + count) `mod` 2 == 1 = [x] ++ luhnSkipper xs (count + 1)
    | otherwise = luhnSkipper xs (count + 1)

luhnSkipperer :: Integral a1 => [a2] -> a1 -> [a2]
luhnSkipperer [] _ = []
luhnSkipperer (x:xs) count 
    | count `mod` 2 == 0 = [x] ++ luhnSkipperer xs (count + 1) 
    | otherwise = luhnSkipperer xs (count + 1)
  
doublerer :: Num a => [a] -> [a]
doublerer [] = []
doublerer (x:xs) = [x*2] ++ doublerer xs

takerofnines :: (Ord a, Num a) => [a] -> [a]
takerofnines [] = []
takerofnines (x:xs)
    | x > 9 = [x-9] ++ takerofnines xs  
    | otherwise = [x] ++ takerofnines xs

puttogether :: Integral a => [a] -> Bool
puttogether input =  (sum (reverse(takerofnines (doublerer (luhnSkipper (luhnReverser input) 0))) ++ luhnSkipperer (reverse input) 0) `mod` 10) == 0


squareOddCubeEven :: Integer
squareOddCubeEven = sum([ x^2  | x <- [0..100],  even x ] ++ [ y^3 | y <- [0..100], odd y])

replicate' :: (Num t, Enum t) => t -> a -> [a]
replicate' n x = [ y | _ <- [0..n-1], y <- [x] ]

pyths :: (Num c, Eq c, Enum c) => c -> [(c, c, c)]
pyths n = [ (a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

factors :: Integral a => a -> [a]
factors n = [ x | x <- [1..n-1], n `mod` x == 0]

perfects :: Integral a => a -> [a]
perfects limit =  [ x | x <- [1..limit-1], x == sum(factors x)]




positions n xs = [ x | (element,x) <- zip xs [0..], element == n]

findf :: Eq a => a -> [ (a,b)] -> [b]
findf k t = [ v | (k',v) <- t, k==k']

pos :: (Eq a1, Num a2, Enum a2) => a1 -> [a1] -> [a2]
pos _ [] = []
pos n xs = findf n (zip xs [0..])

scalar :: Num a => [a] -> [a] -> [a]
scalar xs ys = [ (x+y) | (x,y) <- (zip xs ys) ]

listerer :: (a -> a) -> [a] -> [a]
listerer _ [] = []
listerer func (x:xs) = [(func x)] ++ listerer func xs
    
zipwithh :: (a -> a -> a) -> [a] -> [a] -> [a]
zipwithh f [] _ = []
zipwithh f _ [] = []
zipwithh f (x:xs) (y:ys) = [(f x y)] ++ zipwithh f xs ys

iteratee :: (a -> a) -> a -> [a]
iteratee f x = x : map f (iteratee f x)

reversee :: [a] -> [a]
reversee [] = []
reversee (x:xs) = reversee xs ++ [x]

alll :: (a -> Bool) -> [a] -> Bool
alll _ [] = False
alll f [x] = if f x then True else False
alll f (x:xs)  
    | f x = alll f xs
    | otherwise = False


ally :: (a -> Bool) -> [a] -> Bool
ally _ [] = False
ally f xs = if False `elem` (map f (xs)) then False else True

allier :: (a -> Bool) -> [a] -> Bool
allier _ [] = False
allier f xs 
    | length(filter (f) xs) >= 1 = True
    | otherwise = False


dec2Int :: Num p => [p] -> p
dec2Int [] = 0
dec2Int xs = foldl (\x y -> (10*x)+y) 0 xs

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = []

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap p q [] = []
funkyMap p q [x] = [p x]
funkyMap p q (x : y : xs) = p x : q y : funkyMap p q xs

data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)

traverseer :: Nat -> Int
traverseer Zero = 0
traverseer (Succ a) = 1 + traverseer a


incrementNat :: Nat -> Nat
incrementNat Zero = Succ Zero
incrementNat (Succ a) = Succ(incrementNat a)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat 1 = incrementNat Zero
intToNat n = incrementNat (intToNat (n-1))

evenNat :: Nat -> Bool
evenNat Zero = True
evenNat (Succ a)  = even ((traverseer a)-1)

oddNat :: Nat -> Bool
oddNat Zero = False
oddNat (Succ a) = odd ((traverseer a)-1)

addNat :: Nat -> Nat -> Nat
addNat (Succ a) (Succ b) = intToNat(((traverseer a)+1) + ((traverseer b)+1))

multNat :: Nat -> Nat -> Nat
multNat (Succ a) (Succ b) = intToNat(((traverseer a)+1) * ((traverseer b)+1))

--EXERCISE A4

type Point a = (a,a) 
type Metric a = Point a -> Point a -> Double



accessTuple :: Integer -> (a ,a) -> Double
accessTuple 1 (a,b) = a
accessTuple 2 (a,b) = b


metric :: Metric a
metric a b = accessTuple 1 a   -- (fst b - fst a)^2 + (snd b - snd a)^2

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours _ _ _ _= []
