-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables,
             FlexibleContexts, FlexibleInstances #-}

-- | Implementation of static dictionaries based on FKS hashing schemes.
module Data.Dict.Linear(
       module Data.Dict,
       LinearDict,
       dict,
       ) where

import Control.Monad
import Data.Bits
import Data.Dict
import Data.IORef
import Data.Maybe
import Data.Vector(Vector)
import System.IO.Unsafe
import System.Random

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Mutable
import qualified Data.Vector.Unboxed as Unboxed

-- | Static dictionary based on linear probing with a tabulation hash
-- function.
data LinearDict keyty elemty =
  LinearDict {
    -- | Pointer to the tabulation hash function.
    dictHash :: !(Unboxed.Vector Int),
    -- | The entry hash table
    dictEntries :: !(Vector (Maybe (keyty, elemty)))
  }

hashfuncs :: IORef [Unboxed.Vector Int]
hashfuncs = unsafePerformIO $! newIORef []

addhash :: Unboxed.Vector Int -> IO ()
addhash hashfun = modifyIORef hashfuncs (++ [hashfun])

dict :: Enum keyty =>
        [(keyty, elemty)] ->
        IO (LinearDict keyty elemty)
dict alist =
  let
    alpha = 4
    arrlen = length alist * alpha

    -- Try this hash function, succeed if there a
    tryhash arr hasharr =
      let
        addelems (assoc @ (key, _) : rest) =
          let
            idx = corehash hasharr arrlen key

            -- Linear probe insert
            lininsert offset
                -- Fix alpha to be the bucket size
              | offset < alpha =
                do
                  ent <- Mutable.read arr (idx + offset)
                  -- Check this entry
                  case ent of
                    -- If we collide, try the next index
                    Just _ -> lininsert (offset + 1)
                    -- If we find an open slot, use it
                    Nothing ->
                      do
                        Mutable.write arr (idx + offset) $! Just assoc
                        addelems rest
                -- If we go to far, fail
              | otherwise = return Nothing
          in
            lininsert 0
        -- If we make it here, we're done.
        addelems [] =
          do
            -- Add the hash function
            addhash hasharr
            -- Freeze the array and return it
            frozen <- Vector.unsafeFreeze arr
            return $! Just (hasharr, frozen)
      in do
        -- Clear the array
        forM_ [0..arrlen - 1] (\i -> Mutable.write arr i Nothing)
        addelems alist

    -- Try hash functions until one succeeds
    genhash arr =
      do
        hasharr <- Unboxed.replicateM 0x800 randomIO
        res <- tryhash arr hasharr
        case res of
          Just out -> return out
          Nothing -> genhash arr

    -- If we run out of hash functions, switch over to genhash
    tryall arr [] = genhash arr
    tryall arr (first : rest) =
      do
        -- Try the hash function
        res <- tryhash arr first
        case res of
          -- If it works, return it
          Just out -> return out
          -- Otherwise, move on
          Nothing -> tryall arr rest
  in do
    arr <- Mutable.new arrlen
    hashes <- readIORef hashfuncs
    (hasharr, entarr) <- tryall arr hashes
    return LinearDict { dictEntries = entarr, dictHash = hasharr }

tabhash :: (Enum keyty) => LinearDict keyty elemty -> keyty -> Int
tabhash LinearDict { dictHash = hasharr, dictEntries = ents } key =
  corehash hasharr (length ents) key

corehash :: Enum keyty => Unboxed.Vector Int -> Int -> keyty -> Int
corehash hasharr nents key =
  let
    keyval = fromEnum key
    -- Fragment the key
    k0 = keyval .&. 0xff
    k1 = (keyval `shiftR` 8) .&. 0xff
    k2 = (keyval `shiftR` 16) .&. 0xff
    k3 = (keyval `shiftR` 24) .&. 0xff
    k4 = (keyval `shiftR` 32) .&. 0xff
    k5 = (keyval `shiftR` 40) .&. 0xff
    k6 = (keyval `shiftR` 48) .&. 0xff
    k7 = (keyval `shiftR` 56) .&. 0xff
    -- Get the index parts
    i0 = hasharr Unboxed.! k0
    i1 = hasharr Unboxed.! k1 + 0x100
    i2 = hasharr Unboxed.! k2 + 0x200
    i3 = hasharr Unboxed.! k3 + 0x300
    i4 = hasharr Unboxed.! k4 + 0x400
    i5 = hasharr Unboxed.! k5 + 0x500
    i6 = hasharr Unboxed.! k6 + 0x600
    i7 = hasharr Unboxed.! k7 + 0x700

    hashcode = i0 `xor` i1 `xor` i2 `xor` i3 `xor` i4 `xor` i5 `xor` i6 `xor` i7
  in
    hashcode `mod` nents

instance (Enum keyty, Eq keyty) => Dict keyty LinearDict where
  member d @ LinearDict { dictEntries = ents } key =
    let
      entslen = length ents

      -- Linear probe function
      probe idx
        | idx < entslen =
          case ents Vector.! idx of
            -- If we find the key, we're done
            Just (key', _) | key == key' -> True
            -- If we find nothing, it's not there
            Nothing -> False
            -- Otherwise, check the next slot
            _ -> probe (idx + 1)
        | otherwise = False
    in
      probe (tabhash d key)

  lookup d @ LinearDict { dictEntries = ents } key =
    let
      entslen = length ents

      -- Linear probe function
      probe idx
        | idx < entslen =
          case ents Vector.! idx of
            -- If we find the key, we're done
            Just (key', ent) | key == key' -> Just ent
            -- If we find nothing, it's not there
            Nothing -> Nothing
            -- Otherwise, check the next slot
            _ -> probe (idx + 1)
        | otherwise = Nothing
    in
      probe (tabhash d key)

  assocs = catMaybes . Vector.toList . dictEntries

instance Functor (LinearDict keyty) where
  fmap f LinearDict { dictEntries = ents, dictHash = hasharr } =
    LinearDict { dictEntries = fmap (fmap (fmap f)) ents, dictHash = hasharr }

instance Foldable (LinearDict keyty) where
  foldMap f = foldMap (foldMap (foldMap f)) . dictEntries

instance Traversable (LinearDict keyty) where
  traverse f l @ LinearDict { dictEntries = ents } =
    (\ents' -> l { dictEntries = ents' }) <$>
      traverse (traverse (traverse f)) ents
