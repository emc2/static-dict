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
module Data.Dict.FKS(
       module Data.Dict,
       FKSDict,
       dict,
       ) where

import Data.Array(Array)
import Data.Array.BitArray(BitArray)
import Data.Array.BitArray.IO(IOBitArray)
import Data.Array.IO(IOArray)
import Data.Bits
import Data.Dict
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Word
import Prelude hiding (lookup)
import System.Random

import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.Array.Unsafe as Unsafe
import qualified Data.Array.BitArray as BitArray
import qualified Data.Array.BitArray.IO as IOBitArray

-- | Datatype for a bucket.
data Bucket keyty elemty =
    -- | A multi-element bucket.  All bucket contents are stored in a
    -- single array.  This data structure stores the offset, length,
    -- and local hash function.
    Bucket {
      -- | Offset into the bucket content array.
      bucketOffset :: !Word32,
      -- | Offset length of the segment in the bucket content array.
      bucketLen :: !Word32,
      -- | Value of a1 in the local hash function.
      bucketA1 :: !Word64,
      -- | Value of a2 in the local hash function.
      bucketA2 :: !Word64,
      -- | Value of b in the local hash function.
      bucketB :: !Word64
    }
    -- | Optimization for a single-element bucket.
  | Single {
      -- | Single key being held here.
      singleKey :: !keyty,
      -- | Single element being held here.
      singleElem :: !elemty
    }
    -- | An empty bucket
  | Empty
    deriving Show

-- | Static dictionary over 'Word's, with @elemty@ as element data.
data FKSDict keyty elemty =
    -- | Hash table with only a single level.
    Simple {
      -- | Simple hash table, no collisions
      simpleBuckets :: !(Array Word32 (Maybe (keyty, elemty))),
      -- | Value of a1 in the local hash function.
      simpleA1 :: !Word64,
      -- | Value of a2 in the hash function.
      simpleA2 :: !Word64,
      -- | Value of b in the hash function.
      simpleB :: !Word64
    }
  | Dict {
      -- | First-level array containing buckets.
      dictBuckets :: !(Array Word32 (Bucket keyty elemty)),
      -- | Second-level array containing actual entries.
      dictEntries :: !(Array Word32 (keyty, elemty)),
      -- | Bit array indicating whether or not an index in the entries
      -- array contains anything.
      dictEntryMask :: !(BitArray Word32),
      -- | Value of a1 in the hash function.
      dictA1 :: !Word64,
      -- | Value of a2 in the hash function.
      dictA2 :: !Word64,
      -- | Value of b in the hash function.
      dictB :: !Word64
    }

corehash :: Enum keyty => Word64 -> Word64 -> Word64 -> keyty -> Word32
corehash a1 a2 b key =
  let
    -- Convert to a Word64
    keyword :: Word64
    keyword = fromIntegral (fromEnum key)

    -- Split the key into upper and lower portions
    x1 = keyword .&. 0xffffffff
    x2 = keyword `shiftR` 32
  in
    fromIntegral (((a1 + x1) * (a2 + x2) + b) `shiftR` 32)

-- | Hash a value by the dictionary hash.
dicthash :: Enum keyty => FKSDict keyty elemty -> keyty -> Word32
dicthash Simple { simpleBuckets = arr, simpleA1 = a1,
                  simpleA2 = a2, simpleB = b } key =
  let
    hashcode = corehash a1 a2 b key
    len = snd (Array.bounds arr) + 1
  in
    hashcode `mod` len
dicthash Dict { dictBuckets = arr, dictA1 = a1, dictA2 = a2, dictB = b } key =
  let
    hashcode = corehash a1 a2 b key
    len = snd (Array.bounds arr) + 1
  in
    hashcode `mod` len

-- | Hash a value by the dictionary hash.
buckethash :: Enum keyty => Bucket keyty elemty -> keyty -> Word32
buckethash Bucket { bucketLen = len, bucketA1 = a1,
                    bucketA2 = a2, bucketB = b } key =
  let
    hashcode = corehash a1 a2 b key
  in
    hashcode `mod` len
buckethash _ _ = error "Hashing on invalid bucket!"

dict :: forall keyty elemty .
        (Enum keyty, Show keyty, Show elemty) =>
        [(keyty, elemty)]
     -- ^ List of key/value pairs from which to build the dictionary
     -> IO (FKSDict keyty elemty)
dict alist =
  let
    alpha = 2
    beta = 4
    nassocs = fromIntegral (length alist)

    bucketsize len = (len * len * alpha)

    -- Make a hash function
    makehash :: IO (Word64, Word64, Word64)
    makehash =
      do
        a1 <- randomIO
        a2 <- randomIO
        b <- randomIO
        return (a1, a2, b)

    makebuckets :: Enum keyty => IO (Word64, Word64, Word64, Word32,
                                     Array Word32 (Word32, [(keyty, elemty)]))
    makebuckets =
      let
        -- Insert an entry in its bucket.
        addent :: Enum keyty => Word64 -> Word64 -> Word64 ->
                  IOArray Word32 (Word32, [(keyty, elemty)]) ->
                  (keyty, elemty) -> IO ()
        addent a1 a2 b arr ent @ (key, _) =
          let
            hashcode = corehash a1 a2 b key
            idx = hashcode `mod` fromIntegral nassocs
          in do
            (_, ents) <- IOArray.readArray arr idx
            IOArray.writeArray arr idx (0, ent : ents)

        -- Calculate offsets for buckets and the squared sum
        addoffset :: IOArray Word32 (Word32, [(keyty, elemty)]) ->
                     Word32 -> (Word32, Word32) -> IO (Word32, Word32)
        addoffset arr idx (sqrsum, offset) =
          do
            (_, blist) <- IOArray.readArray arr idx
            case blist of
              -- If there are no entries in the bucket, then leave
              -- the squared sum and offsets alone.
              [] -> return (sqrsum, offset)
              -- If there is a single entry, increment the count,
              -- but leave the offset alone.  Don't update the
              -- entry, as it won't need an offset.
              [_] -> return (sqrsum + 1, offset)
              -- Most of the work happens if there are multiple
              -- entries.
              _ ->
                let
                  blen = fromIntegral (length blist)
                in do
                  -- Set the offset for this bucket to the current offset.
                  IOArray.writeArray arr idx (offset, blist)
                  -- Update the squared sum and offset.
                  return (sqrsum + (blen * blen),
                          offset + bucketsize blen)
      in do
        -- Generate a hash function.
        (a1, a2, b) <- makehash
        arr <- IOArray.newArray (0, nassocs - 1) (0, [])
        -- Try to build buckets with the hash function.
        mapM_ (addent a1 a2 b arr) alist
        -- Count the squared bucket size
        (sqrsum, offset) <- foldrM (addoffset arr) (0, 0) [0 .. nassocs - 1]
        -- Check that the squared sum is less than beta times the length
        if sqrsum <= nassocs * beta
          -- If we succeeded, then freeze the array.
          then do
            frozen <- Unsafe.unsafeFreeze arr
            return (a1, a2, b, offset, frozen)
          -- Otherwise, try again with a different hash function.
          else makebuckets

    makebucket :: IOArray Word32 (keyty, elemty) -> IOBitArray Word32 ->
                  (Word32, [(keyty, elemty)]) -> IO (Bucket keyty elemty)
    -- If the bucket list is empty, convert it to an empty bucket.
    makebucket _ _ (_, []) = return Empty
    -- If the bucket has one entry, generate a single entry.
    makebucket _ _ (_, [(key, ent)]) =
      return Single { singleKey = key, singleElem = ent }
    makebucket entarr bitarr (offset, blist) =
      let
        bsize = bucketsize (fromIntegral (length blist))

        -- Clear out a range of the bit array
        reset :: IO ()
        reset =
          let
            clearbit idx = IOBitArray.writeArray bitarr idx False
          in
            mapM_ clearbit [offset..offset + bsize - 1]

        -- Generate a hash function with no collisions.
        genhash :: IO (Word64, Word64, Word64)
        genhash =
          let
            setbit :: Word64 -> Word64 -> Word64 ->
                      (keyty, elemty) -> Bool -> IO Bool
            setbit _ _ _ _ False = return False
            setbit a1 a2 b (key, _) _ =
              let
                hashcode = corehash a1 a2 b key
                idx = (hashcode `mod` bsize) + offset
              in do
                -- Check for a collision
                collide <- IOBitArray.readArray bitarr idx
                if collide
                  -- If there's a collision, fail
                  then return False
                  -- Otherwise, set the bit and continue
                  else do
                    IOBitArray.writeArray bitarr idx True
                    return True
          in do
            -- Generate the hash function
            (a1, a2, b) <- makehash
            -- Check for collisions
            ok <- foldrM (setbit a1 a2 b) True blist
            if ok
              -- If the hash function is ok, then return it
              then return (a1, a2, b)
              -- Otherwise, reset and try another hash function.
              else do
                reset
                genhash


        addent :: Word64 -> Word64 -> Word64 -> (keyty, elemty) -> IO ()
        addent a1 a2 b ent @ (key, _) =
          let
            hashcode = corehash a1 a2 b key
            idx = (hashcode `mod` bsize) + offset
          in
            IOArray.writeArray entarr idx ent
      in do
        -- Generate a hash function
        (a1, a2, b) <- genhash
        -- Write everything into the array
        mapM_ (addent a1 a2 b) blist
        return Bucket { bucketA1 = a1, bucketA2 = a2, bucketB = b,
                        bucketOffset = offset, bucketLen = bsize }

    singlebucket (_, []) = Nothing
    singlebucket (_, [ent]) = Just ent
    singlebucket _ = error "Should not see multi-entry bucket"

    makeentries :: Word32 -> Array Word32 (Word32, [(keyty, elemty)]) ->
                   IO (Array Word32 (Bucket keyty elemty),
                       Array Word32 (keyty, elemty),
                       BitArray Word32)
    makeentries 0 _ = error "Should not see zero-length array"
    makeentries entrieslen bucketsarr =
      do
        entarray <- IOArray.newArray_ (0, entrieslen - 1)
        bitarray <- IOBitArray.newArray (0, entrieslen - 1) False
        entries <- mapM (makebucket entarray bitarray) bucketsarr
        frozenents <- Unsafe.unsafeFreeze entarray
        frozenbits <- IOBitArray.unsafeFreeze bitarray
        return (entries, frozenents, frozenbits)
  in do
    (a1, a2, b, offset, barray) <- makebuckets
    if 0 /= offset
      then do
        (buckets, ents, emask) <- makeentries offset barray
        return Dict { dictBuckets = buckets, dictEntries = ents,
                      dictEntryMask = emask, dictA1 = a1, dictA2 = a2,
                      dictB = b }
      else return Simple { simpleBuckets = fmap singlebucket barray,
                           simpleA1 = a1, simpleA2 = a2, simpleB = b }

instance (Enum keyty, Eq keyty) => Dict keyty FKSDict where
  member s @ Simple { simpleBuckets = buckets } key =
    let
      bucketidx = dicthash s key
    in case buckets Array.! bucketidx of
      Just (key', _) | key == key' -> True
      _ -> False
  member d @ Dict { dictBuckets = buckets, dictEntries = entries,
                    dictEntryMask = emask } key =
    let
      bucketidx = dicthash d key
    in
      -- Check the buckets array for what to do.
      case buckets Array.! bucketidx of
        -- If we get a proper bucket, run the second hash function and
        -- check the entries array.
        b @ Bucket { bucketOffset = offset } ->
          let
            entryidx = (buckethash b key) + offset
          in
            -- Check if the entries array actually contains something at
            -- the second hash index.
            if emask BitArray.! entryidx
            then
              -- If it does, then check that the key is correct.
              let
                (key', _) = entries Array.! entryidx
              in
                key' == key
            -- Otherwise, there's no such entry
            else False
        -- For a single entry, check if the key is right.
        Single { singleKey = key' } -> key == key'
        -- For an empty bucket, return false
        Empty -> False

  lookup s @ Simple { simpleBuckets = buckets } key =
    let
      bucketidx = dicthash s key
    in
      -- Check that the entry exists and the key is equal
      case buckets Array.! bucketidx of
        Just (key', ent) | key == key' -> Just ent
        _ -> Nothing
  lookup d @ Dict { dictBuckets = buckets, dictEntries = entries,
                    dictEntryMask = emask } key =
    let
      bucketidx = dicthash d key
    in
      -- Check the buckets array for what to do.
      case buckets Array.! bucketidx of
        -- If we get a proper bucket, run the second hash function and
        -- check the entries array.
        b @ Bucket { bucketOffset = offset } ->
          let
            entryidx = (buckethash b key) + offset
          in
            -- Check if the entries array actually contains something at
            -- the second hash index.
            if emask BitArray.! entryidx
            then
              -- If it does, then check that the key is correct.
              case entries Array.! entryidx of
                -- If the key matches, we have the entry.
                (key', ent) | key' == key -> Just ent
                -- Otherwise, there's no such entry.
                _ -> Nothing
            -- Otherwise, there's no such entry
            else Nothing
        -- For a single entry, check if the key is right.
        Single { singleKey = key', singleElem = ent } | key == key' -> Just ent
        -- For anything else, return Nothing
        _ -> Nothing

  assocs Simple { simpleBuckets = buckets } =
    catMaybes (Array.elems buckets)
  assocs Dict { dictEntryMask = emask, dictEntries = entries,
                dictBuckets = buckets } =
    let
      entryfold (idx, True) accum = entries Array.! idx : accum
      entryfold (_, False) accum = accum

      bucketfold Single { singleKey = key, singleElem = ent } accum =
        (key, ent) : accum
      bucketfold Bucket {} accum = accum
      bucketfold Empty accum = accum
    in
      foldr entryfold (foldr bucketfold [] buckets) (BitArray.assocs emask)

instance Functor (Bucket keyty) where
  fmap _ Bucket { bucketOffset = offset, bucketLen = len, bucketA1 = a1,
                  bucketA2 = a2, bucketB = b } =
    Bucket { bucketOffset = offset, bucketLen = len, bucketA1 = a1,
             bucketA2 = a2, bucketB = b }
  fmap f Single { singleKey = key, singleElem = ent } =
    Single { singleKey = key, singleElem = f ent }
  fmap _ Empty = Empty

instance Functor (FKSDict keyty) where
  fmap f Simple { simpleBuckets = buckets, simpleA1 = a1,
                  simpleA2 = a2, simpleB = b } =
    Simple { simpleBuckets = fmap (fmap (\(key, ent) -> (key, f ent))) buckets,
             simpleA1 = a1, simpleA2 = a2, simpleB = b }
  fmap f Dict { dictEntries = entries, dictEntryMask = emask, dictB = b,
                dictBuckets = buckets, dictA1 = a1, dictA2 = a2 } =
    let
      foldfun accum' (idx, True) =
        let
          (key, ent) = entries Array.! idx
        in
          (idx, (key, f ent)) : accum'
      foldfun accum' (_, False) = accum'

      newassocs = foldl foldfun [] (BitArray.assocs emask)
    in
      Dict { dictEntries = Array.array (Array.bounds entries) newassocs,
             dictEntryMask = emask, dictBuckets = fmap (fmap f) buckets,
             dictA1 = a1, dictA2 = a2, dictB = b }

instance Foldable (Bucket keyty) where
  foldMap f Single { singleElem = ent } = f ent
  foldMap _ _ = mempty

instance Foldable (FKSDict keyty) where
  foldMap f Simple { simpleBuckets = buckets } =
    foldMap (foldMap (f . snd)) buckets
  foldMap f Dict { dictEntries = entries, dictEntryMask = emask,
                   dictBuckets = buckets } =
    let
      foldfun (idx, True) = f (snd (entries Array.! idx))
      foldfun (_, False) = mempty
    in
      foldMap (foldMap f) buckets `mappend`
      foldMap foldfun (BitArray.assocs emask)

instance Traversable (Bucket keyty) where
  traverse f s @ Single { singleElem = ent } =
    (\ent' -> s { singleElem = ent' }) <$> f ent
  traverse _ Bucket { bucketOffset = offset, bucketLen = len,
                      bucketA1 = a1, bucketA2 = a2, bucketB = b } =
    pure Bucket { bucketOffset = offset, bucketLen = len,
                  bucketA1 = a1, bucketA2 = a2, bucketB = b }
  traverse _ Empty = pure Empty

instance Traversable (FKSDict keyty) where
  traverse f s @ Simple { simpleBuckets = buckets } =
    (\newbuckets -> s { simpleBuckets = newbuckets }) <$>
    traverse (traverse (traverse f)) buckets
  traverse f d @ Dict { dictEntries = entries, dictEntryMask = emask,
                        dictBuckets = buckets } =
    let
      foldfun accum' (idx, True) =
        let
          (key, ent) = entries Array.! idx
        in
          (\val ents -> (idx, (key, val)) : ents) <$>
            (f ent) <*> accum'
      foldfun accum' (_, False) = accum'

      arrbounds = Array.bounds entries
    in
      (\newassocs buckets' -> d { dictEntries = Array.array arrbounds newassocs,
                                  dictBuckets = buckets' }) <$>
        foldl foldfun (pure []) (BitArray.assocs emask) <*>
        traverse (traverse f) buckets
