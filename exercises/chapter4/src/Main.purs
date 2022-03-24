module Main where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array (cons, filter, foldl, head, length, null, range, tail)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Math ((%))

mylength :: forall a. Array a -> Int
mylength arr =
  if null arr then
    0
  else
    1 + (mylength $ fromMaybe [] $ tail arr)

type People = {
  name :: String
  ,age :: Number
}

type PeopleBook = Array People

emptyPeopleBook :: PeopleBook
emptyPeopleBook = empty

emptyInt :: Array Int
emptyInt = empty

insertEntry :: People -> PeopleBook -> PeopleBook
insertEntry = cons

insertInt :: Int -> Array Int -> Array Int
insertInt = cons

john :: People
john =
  { name: "John"
  , age: toNumber 6
  }
sam :: People
sam =
  { name: "Sam"
  , age: toNumber 9
  }

book :: PeopleBook
book =
  insertEntry john
    $ insertEntry sam
    $ insertEntry john
        emptyPeopleBook

lotsInts :: Array Int
lotsInts =
  insertInt 3
    $ insertInt 2
    $ insertInt 3
        emptyInt


isEven :: Int -> Boolean
isEven num = toNumber num % toNumber 2 == toNumber 0

countEvenHelper :: Int -> Array Int -> Int
countEvenHelper count arr =
  if null arr then
    count
  else
    if isEven $ fromMaybe 1 $ head arr then
      countEvenHelper (count + 1) (fromMaybe [] $ tail arr)
    else
      countEvenHelper count (fromMaybe [] $ tail arr)

-- countEven :: Array Int -> Int
-- countEven arr = countEvenHelper 0 arr

boolMapper :: Boolean -> Int
boolMapper bool = if bool then 1 else 0

countEven :: Array Int -> Int
countEven arr =
  if null arr then 0
  else boolMapper (isEven $ fromMaybe 1 $ head arr) + countEven (fromMaybe [] $ tail arr)


squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative numbers = filter helper numbers
  where
    helper :: Number -> Boolean
    helper num = if num >= 0.0 then true else false


myfactors :: Int -> Array (Array Int)
myfactors n = do
  i <-  1 `range` n
  j <- i `range` n
  guard $ i * j == n
  pure [ i, j ]

isPrime :: Int -> Boolean
isPrime num = if num == 1 then false else (length $ myfactors num) == 1

  
cartesianProduct ::forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
  i <- arr1
  j <- arr2
  pure [i, j]

triples :: Int -> Array (Array Int)
triples num = do
  i <- range 1 num
  j <- range i num
  k <- range j num
  guard $ i * i + j * j == k * k
  [[i,j,k]]

primeFactors :: Int -> Array Int
primeFactors num = factorize 2 num
  where
    factorize :: Int -> Int -> Array Int
    factorize _ 1 = []
    factorize dinominator numarator = 
      if mod numarator dinominator == 0 then
        cons dinominator $ factorize dinominator (numarator / dinominator)
      else
        factorize (dinominator + 1) numarator


allTrue :: Array Boolean -> Boolean
allTrue bools = foldl (\acc bool -> acc && bool) true bools


fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)


main :: Effect Unit
main = do
  log ("Is 9 even :" <> (show ( isEven 9)))
  log ("Is 12 even :" <> (show ( isEven 12)))
  log $ show $ isEven 95686
  log $ show $ countEvenHelper 0 lotsInts
  log $ show $ squared [1.0,2.0,3.0]
  log $ show $ keepNonNegative [1.0,2.0,3.0, -35.0]
  log $ show $ isPrime 9
  log $ show $ cartesianProduct [1.0,2.0,3.0] [1.0,2.0,3.0]
  log $ show $ triples 10 
  log $ show $ allTrue [true, true, true]
  log "🍝"
  
