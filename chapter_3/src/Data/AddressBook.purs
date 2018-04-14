module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

{-
Javascript primitives to PureScript types:
  - numbers => Number
  - strings => String
  - booleans => Boolean
PureScript does offer some other built-in types:
  - arrays (⚠️: homogeneous),
  - characters,
  - integers,
  - records
  - functions

  Some examples: [1,2,3] is of type Array Int
  ==
  Record syntax have the same syntax as Javascript objects

  Quantified types using flip explanation:
  Here is the signature of the function to be flipped:
    \n s -> show n <> s
  Now because we are passing in "Ten" and 10 to the *flipped*
  function, PureScript will infer the type of the above closure to be:
  Int -> String -> String (unflipped version)

  Applying flip on the closure flips (obviously) the inputs to:
  String -> Int -> String
  and this is why we are passing in a string first ("Ten")
  then a number/int (10)

  Type constructors
  ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
  List is an example of a type constructor. Here are some PSCi output:

  > :kind Number
  > Type

  > import Data.List
  > : kind List
  > Type -> Type

  > :kind List String
  > Type
-}
type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

-- | Render an Entry as a String (description)
showEntry :: Entry -> String
showEntry e =
  e.firstName <> ", " <>
  e.lastName <> ": " <>
  showAddress e.address

-- | Render an Address as a String (description)
showAddress :: Address -> String
showAddress a =
  a.street <> ", " <>
  a.city <> ", " <>
  a.state

emptyBook :: AddressBook
emptyBook = empty -- Initializing to empty AddressBook (List Entry)

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons
  -- eta conversion where the arguments are not specified

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName =
  head <<< filter filterEntry -- filter filterEntry >>> head
    where
      filterEntry :: Entry -> Boolean
      filterEntry entry =
        entry.firstName == firstName
        && entry.lastName == lastName
{-
Here is how to test:
> address = {street: "14A Coriolis Road", city: "Bananes", state: "Grand-Port"}
> bonzy = {firstName: "Bonzy", lastName: "Jhoomuck", address: address } :: Entry
> book1 = insertEntry bonzy emptyBook
> map showEntry $ findEntry "Bonzy" "Jhoomuck" book1
-}

{-
Exercise 1: (Easy)
Test your understanding of the findEntry function by writing down
the types of each of its major subexpressions.
For example, the type of the head function as used is specialized to
AddressBook -> Maybe Entry.

ANSWER:
head :: AddressBook -> Maybe Entry
filterEntry :: Entry -> Boolean
filter filterEntry :: AddressBook -> AddressBook
...anything else?

-- Example to test it in the REPL
ff = filter filterEntry -- filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry =
      entry.firstName == "firstName"
      && entry.lastName == "lastName"
-}

{-
Exercise 2
Write a function which looks up an Entry
given a street address,
by reusing the existing code in findEntry.
Test your function in PSCi

ANSWER:
-}
entryFromStreetAddress :: String -> AddressBook -> Maybe Entry
entryFromStreetAddress street =
  head <<< filter filterUsingAddress
    where
      filterUsingAddress :: Entry ->Boolean
      filterUsingAddress e = e.address.street == street

{-
Exercise 3
ˆˆˆˆˆˆˆˆˆˆ
> address = { street: "straat", city:"Amsterdam", state: "1018GW" }
> bonzy = { firstName: "Bonzy", lastName: "Jhoomuck", address: address } :: Entry
> paro = { firstName: "Paro", lastName: "Jhoomuck", address: address } :: Entry
> jmj = { firstName: "Vandhana", lastName: "Teeluckdharry", address: address } :: Entry
> b1 = insertEntry bonzy emptyBook
> b2 = insertEntry paro b1
> b3 = insertEntry jmj b2
ANSWER:
-}
contains :: String -> String -> AddressBook -> Boolean
contains firstName lastName = not null <<< filter filterEntry
  where
      filterEntry :: Entry -> Boolean
      filterEntry entry =
        entry.firstName == firstName
        && entry.lastName == lastName

{-
Exercise 4
ˆˆˆˆˆˆˆˆˆˆ
ANSWER:
-}
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy namePred
  where
    namePred :: Entry -> Entry -> Boolean
    namePred e1 e2 =
      e1.firstName == e2.firstName &&
      e1.lastName == e2.lastName