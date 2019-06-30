-- Copyright 2019 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}

module Codelab where

import Control.Monad (void)
import Data.Maybe (isJust)
import Prelude hiding
  ( (++)
  , and
  , filter
  , foldl
  , foldr
  , gcd
  , head
  , length
  , map
  , null
  , or
  , tail
  )
import Text.Read (readMaybe)

codelab :: a
codelab = error "SOMETHING IS NOT IMPLEMENTED!"

{- #####################################################################
   SECTION 1: number manipulation

   As we have not looked at any complex data structures yet, so the
   only thing we have for now are numbers.
-}
add :: Int -> Int -> Int
add x y = x + y

subtract :: Int -> Int -> Int
subtract x y = x - y

double :: Int -> Int
double x = x * 2

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Double
divide x 0 = error "divide by zero"
divide x y = fromIntegral x / fromIntegral y

factorial :: Integer -> Integer
factorial n =
  if n < 0
    then error "factorial only defined for natural numbers"
    else foldl (*) 1 [1 .. n]

gcd :: Int -> Int -> Int
gcd 0 b = b
gcd a b = gcd (b `mod` a) a

{- #####################################################################
   SECTION 2: simple pattern matching

   Not that we can define simple data structures, let's try using them.
-}
data Minutes =
  Minutes Int

hours :: Minutes -> Int
hours (Minutes m) = m `div` 60

-- Distance here means the number of minutes to get from m1 to m2.  For
-- example, for 15 and 25, distance is 10.
timeDistance :: Minutes -> Minutes -> Minutes
timeDistance (Minutes m) (Minutes n) = Minutes (abs (m - n))

type Point = (Int, Int)

pointDistance :: Point -> Point -> Double
pointDistance (m, n) (u, v) = dist
  where
    x = fromIntegral $ abs (m - u)
    y = fromIntegral $ abs (n - v)
    x2 = x ** 2
    y2 = y ** 2
    dist = sqrt (x2 + y2)

{- #####################################################################
   SECTION 3: deconstructing lists

   The default list is ubiquitous in the Prelude; the default String
   type is but a type alias to [Char] after all. Though they have
   limitations, they're always useful.

   As a reminder, a list is either:
     * []     the empty list
     * (x:xs) a cell containing the value x and followed by the list xs
-}
-- null tells you whether a list is empty or not
null :: [a] -> Bool
null [] = True
null _ = False

-- head returns the first element of the list
-- if the list is empty, it panics: this function is partial
head :: [a] -> a
head [] = error "head: empty list"
head (x:xs) = x

-- tail returns everything but the first element
-- if the list is empty it panics
tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (x:xs) = xs

{- #####################################################################
   SECTION 4: recursion (c.f. SECTION 4)

   There is no loop in Haskell, so to go through a list, we have to use
   recursion. Here are a few more common functions for you to
   reimplement!
-}
length :: [a] -> Int
length xs = foldr (\_ n -> n + 1) 0 xs

-- "and" returns True if all the boolean values in the list are True.
-- prefer first defintion because it is lazier and short circuits
and :: [Bool] -> Bool
and [] = True
and (x:xs) =
  if x == True
    then and xs
    else False

--and xs = foldr (\a acc -> acc && a) True xs
--and xs = foldl True (\a acc -> acc && a) xs
-- "or" returns True if at least one value in the list is True.
-- prefer first defintion because it is lazier and short circuits?
or :: [Bool] -> Bool
or [] = False
or (x:xs) =
  if x == True
    then x
    else or xs

-- and xs = foldr (\a acc -> acc || a) False
-- "(++)" is the concatenation operator.  To concatenate two linked lists
-- you have to chain the second one at the end of the first one.
(++) :: [a] -> [a] -> [a]
l1 ++ [] = l1
[] ++ l2 = l2
(x:xs) ++ l2 = x : (xs ++ l2)

-- map
-- map (+1) [0, 1, 2] == [0 + 1, 1 + 1, 1 + 2] == [1, 2, 3]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = f a : map f as

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
  | (f x) == True = x : filter f xs
  | otherwise = filter f xs

-- foldl
-- foldl (-) 0 [1,2,3,4]   ==   (((0 - 1) - 2) - 3) - 4   ==   -10
foldl :: (a -> x -> a) -> a -> [x] -> a
foldl _ a [] = a
foldl f a (x:xs) = foldl f (f a x) xs

-- foldr
-- foldr (-) 0 [1,2,3,4]   ==   1 - (2 - (3 - (4 - 0)))   ==    -2
foldr :: (x -> a -> a) -> a -> [x] -> a
foldr _ a [] = a
foldr f a (x:xs) = x `f` (foldr f a xs)

{- #####################################################################
   BONUS STAGE!

   For fun, you can try reimplementing all the functions in section 4 with
   foldr or foldl! For length, remember that the syntax for a lambda
   function is (\arg1 arg2 -> value).

   You can replace your previous implementation if you want. Otherwise, you
   can add new functions (such as andF, orF), and test them by loading your
   file in GHCI:

   $ ghci
   > :load Codelab
   > and  []
   > andF []

   To go a bit further, you can also try QuickCheck:

   > import Test.QuickCheck
   > quickCheck $ \anyList -> and anyList == andF anyList

   QuickCheck automatically generates tests based on the types expected
   (here, list of boolean values).

   It is also worth noting that there is a special syntax for list
   comprehension in Haskell, which is at a first glance quite similar to
   the syntax of Python's list comprehension

   Python:  [transform(value) for value in container if test(value)]
   Haskell: [transform value  |   value <- container ,  test value ]

   This allows you to succinctly write your map / filters.
-}
{- #####################################################################
   SECTION 6: am I being indecisive? ....hmmmm Maybe?

   Partial functions are bad. Null pointers are a billion dollar
   mistake. Sometimes, what we just want is to have an optional value, a
   value that is either here or not, but with type safety.

   Remember Maybe? If not, here's the definition:

   data Maybe a = Nothing | Just a
-}
-- If we were to fix the "head" function, how could we do that?
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- "isNothing" should not need an explanation by now!
isNothing :: Maybe a -> Bool
isNothing x =
  case x of
    Just _ -> False
    Nothing -> True

-- The "fromMaybe" function is your way out of a Maybe value.
-- It takes a default value to use in case our Maybe value is Nothing.
fromMaybe :: a -> Maybe a -> a
fromMaybe d x =
  case x of
    Just a -> a
    Nothing -> d

-- The "maybe" function is an extended version of "fromMaybe".  Can you
-- guess what it is supposed to do?
-- ...doesn't it kinda look like fold?
maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

{- #####################################################################
   BONUS SECTION: let's play a game.

   This section goes a bit further and is optional.

   In it, we implement a small (and, arguably, not very interesting)
   game: Rock Paper Scissors! You don't have to write a lot of code in
   this section; simply take the time to read the code, and fill in the
   few blanks. You'll encounter quite a few functions you haven't seen
   before, and some new weird syntax: if you import this file in GHCI,
   you can easily inspect the type of any function with :t.

   To play a game, simply type "play" in GHCI!
   Feel free to try to modify the code and tweak it as you wish.
-}
-- Some simple types for our game.  Ignore the "deriving" part (or don't,
-- I'm a comment, not a cop).
data Hand
  = Rock
  | Paper
  | Scissors
  deriving (Show, Read, Eq)

type Score = (Int, Int)

-- "winsOver" tells you if a hand wins over another one.  It introduces a
-- nifty trick: any binary function can be used in an infix way if
-- surrounded by backquotes.
winsOver :: Hand -> Hand -> Bool
Rock `winsOver` Scissors = True
Paper `winsOver` Rock = True
Scissors `winsOver` Paper = True
_ `winsOver` _ = False

-- "computeScore"... computes the score!
-- Remember those | guards?
computeScore :: Hand -> Hand -> Score
computeScore h1 h2
  | h1 `winsOver` h2 = (1, 0)
  | h2 `winsOver` h1 = (0, 1)
  | otherwise = (0, 0)

-- "combine"... combines!
-- Remember pattern matching?
combine :: Score -> Score -> Score
combine (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

-- Ok, here's where you come in.
--
-- We want to create a function "score", that takes the two lists of hands
-- the players have played, computes the score at each round, then combines
-- all the scores to yield the final count.
--
-- This function is partially pre-defined, using the ($) operator, to
-- showcase how easily you can combine existing functions into new ones.
-- Your job is to figure out which function goes where.
--
-- Here is the list of functions you will need:
--     combine      :: Score -> Score -> Score
--     computeScore :: Hand  -> Hand  -> Score
--     uncurry      :: (a -> b -> c) -> ((a, b) -> c)
--     foldl1       :: (a -> a -> a) -> [a] -> a
--     map          :: (a -> b) -> [a] -> [b]
--     zip          :: [a] -> [b] -> [(a, b)]
pairScore :: (Hand, Hand) -> Score
pairScore = codelab codelab

score :: [Hand] -> [Hand] -> Score
score h1 h2 = codelab codelab $ codelab codelab $ codelab h1 h2

-- Hint: It creates a list of plays by merging the two lists,
--       then it scores each play,
--       then it sums the scores.
--       merge -> map -> reduce
-- We play up to 3.
gameOver :: Score -> Bool
gameOver (s1, s2) = s1 >= 3 || s2 >= 3

-- Below is the impure IO code that lets us read hands from the standard
-- input and play the game!
-- Beware: Haskell 102 spoilers!
readHand :: String -> IO Hand
readHand prompt = do
  putStr prompt -- prints the prompt
  handText <- getLine -- reads one line of input
  case readMaybe handText -- tries to convert it to Hand
        of
    Just h -> return h -- success: our result is h
    Nothing -> readHand prompt -- failure: we try again

playTurn :: Score -> IO Score
playTurn oldScore = do
  h1 <- readHand "p1: "
  h2 <- readHand "p2: "
  let turnScore = computeScore h1 h2
      newScore = combine oldScore turnScore
  print newScore
  if gameOver newScore
    then return newScore
    else playTurn newScore

play :: IO ()
play = void $ playTurn (0, 0)

{- #####################################################################
   BONUS BONUS SECTION: wait, you actually read all of that?

   Just for fun, here are a few common one-liners; can you guess what they
   do, what they are, without testing them in GHCI?
-}
mystic :: [Integer]
mystic = 0 : 1 : zipWith (+) mystic (tail mystic)

valor :: [Integer]
valor =
  let s l = head l : s [n | n <- tail l, n `mod` head l /= 0]
   in s [2 ..]

instinct :: [Int] -> [Int]
instinct [] = []
instinct (x:xs) =
  instinct [a | a <- xs, a < x] ++ [x] ++ instinct (filter (>= x) xs)
-- -*- fill-column: 75; -*-
