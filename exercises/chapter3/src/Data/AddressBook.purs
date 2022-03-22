module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubByEq)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head (filter filterEntry book)
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = _.address.street entry == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- isDuplicate :: Entry -> Entry -> Boolean
-- isDuplicate x y = x.firstName == y.firstName && x.lastName == y.lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq isDuplicate
  where
    isDuplicate :: Entry -> Entry -> Boolean
    isDuplicate x y = x.firstName == y.firstName && x.lastName == y.lastName