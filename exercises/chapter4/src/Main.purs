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
  , length
  , lotsInts
  , main
  , sam
  )
  where

import Prelude

import Control.Plus (empty)
import Data.Array (cons, head, null, tail)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Math ((%))

length :: forall a. Array a -> Int
length arr =
  if null arr then
    0
  else
    1 + (length $ fromMaybe [] $ tail arr)

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

countEven :: Int -> Array Int -> Int
countEven count arr =
  if null arr then
    count
  else
    if isEven $ fromMaybe 1 $ head arr then
      countEven (count + 1) (fromMaybe [] $ tail arr)
    else
      countEven count (fromMaybe [] $ tail arr)


main :: Effect Unit
main = do
  log ("Is 9 even :" <> (show ( isEven 9)))
  log ("Is 12 even :" <> (show ( isEven 12)))
  log $ show $ isEven 95686
  log $ show $ countEven 0 lotsInts
  log "🍝"
  
