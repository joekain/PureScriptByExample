module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }
type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons

findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByPhone :: String -> PhoneBook -> Maybe Entry
findEntryByPhone phone = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.phone == phone


containsEntry :: String -> String -> PhoneBook -> Boolean
containsEntry firstName lastName book = not $ null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: PhoneBook -> PhoneBook
removeDuplicates book = nubBy entryEqual book
  where
    entryEqual :: Entry -> Entry -> Boolean
    entryEqual left right = left.firstName == right.firstName && left.lastName == right.lastName
