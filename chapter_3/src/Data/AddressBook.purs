module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head)
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
