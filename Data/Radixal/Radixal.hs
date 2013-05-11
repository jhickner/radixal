{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Data.Radixal.Radixal where

import Data.Aeson (Value(..), object, (.=), ToJSON(..), toJSON)

import qualified Data.ListTrie.Patricia.Map as Base
import qualified Data.ListTrie.Patricia.Map as TM
import Data.Text (Text)
import Data.List (foldl')
import Control.Arrow (second)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Data.Radixal.HashMap

-- set
-- insert . objectToTrieList $ o
-- traverse o and dispatch each sub-object

-- for incs
-- objectToTrieList on the incoming object
-- incTrieList, which returns a transformed TrieList with inced values
-- dispatch on that

-- locks:
-- need to check lock on each component of the path to a node...
-- still use IxMap?
-- should locks be stored in a Trie too? limits search space...

type TrieMap = Base.TrieMap HashMap
type Rx = TrieMap Text Value
type RxList = [([Text], Value)]

instance ToJSON Rx where
  toJSON = toObject

empty :: Rx
empty = TM.empty

search :: [Text] -> Rx -> Rx
search = TM.lookupPrefix

fromList :: RxList -> Rx
fromList = TM.fromList

fromObject :: Value -> Rx
fromObject = insertRxList TM.empty . objectToRxListWithPrefix [] 

-- | fold an Object into a list of key value pairs we can insert, 
-- with each key prefixed by the given key list
objectToRxListWithPrefix :: [Text] -> Value -> RxList
objectToRxListWithPrefix = go []
  where
    go acc ks (Object o) = foldl' (f ks) acc $ M.toList o
    go acc ks v          = (ks,v) : acc
    f ks acc (k,v)       = go acc (ks ++ [k]) v

-- should always return an Object for valid Tries
toObject :: Rx -> Value
toObject t = wrap prefix mfirst (TM.children t)
  where
    (prefix, mfirst, _) = TM.splitPrefix t 
    wrap []     mf c = case (mf, M.null c) of
      (Just v, True) -> v 
      _              -> object $ pairs c
    wrap [k]    mf c = case (mf, M.null c) of
      (Just v, True) -> object [k .= v]
      -- If there is both a value for this key AND a subobject (an invalid
      -- state that our insert function should prevent) then we give precedence
      -- to the sub object.
      _              -> object [k .= object (pairs c)]
    wrap (k:ks) mf c = object [k .= wrap ks mf c]
    pairs c = map (second toObject) $ M.toList c

insertRxList :: Rx -> RxList -> Rx
insertRxList = foldl' (\t (k,v) -> insert k v t)

-- smart insert 
insert :: [Text] -> Value -> Rx -> Rx
insert ks v t = case v of
    Object o -> foldl' insertObj pruned $ M.toList o
    _        -> TM.insert' ks v pruned 
  where
    pruned = TM.deleteSuffixes ks t
    insertObj t' (k,v') = insert (ks ++ [k]) v' t'

delete :: [Text] -> Rx -> Rx
delete = TM.deleteSuffixes 


-- printRx t = putStrLn $ TM.showTrie t ""
  
