module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 1 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x,y] = [x * 2, y]
doubleEveryOther [x,y,z] = [x, y * 2, z]
doubleEveryOther (x:y:zs) = x * 2 : y : doubleEveryOther zs

-- [8,7,6,5]
-- [16, 7, doubleEveryOther [6, 5]]
-- [16, 7, 12, 5]


-- doubleEveryOther (x:(y:zs)) = (x : (y * 2)) : doubleEveryOther zs
-- doubleEveryOther reverse(x:(y:zs)) = doubleEveryOther (reverse (x:(y*2:zs)))


-- if not in Haskell I would:
-- iterate over the list, maintaining a count of position in the list
-- if count is even then double the value
-- otherwise leave the value alone


-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | listLength (toDigits(x)) <= 0 = 0
  | listLength (toDigits(x)) == 1 = x
  | listLength (toDigits(x)) > 1 = sumDigits (toDigits (x))
sumDigits (x:xs) = sumDigits ([x]) + sumDigits (xs)

listLength :: [Integer] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


-- #4
validate :: Integer -> Bool
validate x
  | (sumDigits (doubleEveryOther ((toDigits (x)))) `mod` 10) == 0 = True
  | otherwise = False


-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
