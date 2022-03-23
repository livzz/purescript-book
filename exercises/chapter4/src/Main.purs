module Main
  ( People
  , PeopleBook
  , book
  , countEven
  , emptyInt
  , emptyPeopleBook
  , insertEntry
  , insertInt
  , isEven
  , john
  , mylength
  , lotsInts
  , main
  , sam
  )
  where

import Prelude

import Control.Plus (empty)
import Data.Array (cons, filter, head, null, tail)
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


squared :: Array Int -> Array Int
squared = map (\n -> n * 2)

keepNonNegative :: Array Int -> Array Int
keepNonNegative numbers = filter helper numbers
  where
    helper :: Int -> Boolean
    helper num = if num >= 0 then true else false


main :: Effect Unit
main = do
  log ("Is 9 even :" <> (show ( isEven 9)))
  log ("Is 12 even :" <> (show ( isEven 12)))
  log $ show $ isEven 95686
  log $ show $ countEvenHelper 0 lotsInts
  log $ show $ squared [1,2,3]
  log $ show $ keepNonNegative [1,2,3, -35]
  log "🍝"
  
