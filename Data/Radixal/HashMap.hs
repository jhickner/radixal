{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Radixal.HashMap where

import Data.ListTrie.Base.Map
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

-- | Partial Data.ListTries.Base.Map instance for Data.HashMap.Strict
instance (Hashable k, Ord k) => Map HashMap k where
   eqCmp = const (==)

   empty     = M.empty
   singleton = M.singleton

   null   = M.null
   lookup = M.lookup

   insertWith = M.insertWith

   adjust = M.adjust
   delete = M.delete

   alter f k m = case f $ M.lookup k m of
      Nothing -> M.delete k m
      Just v  -> M.insert k v m

   unionWith           = M.unionWith
   intersectionWith    = M.intersectionWith

   -- TODO: actually implement these (although we don't need them)
   unionWithKey        = undefined
   differenceWithKey   = undefined
   intersectionWithKey = undefined
   isSubmapOfBy        = undefined

   map             = M.map
   filter = M.filter

   toList       = M.toList
   fromList     = M.fromList
   fromListWith = M.fromListWith

   serializeToList     = M.toList
   deserializeFromList = M.fromList
