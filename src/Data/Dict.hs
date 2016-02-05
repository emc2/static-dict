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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Interface for static dictionaries.  These are associative data
-- structures wich do not support dynamic updates, and support a
-- simpler range of operations than structures such as 'IntMap' or
-- 'Map'.
module Data.Dict(
       Dict(..)
       ) where

-- | Typeclass for static dictionaries.
class Dict keyty dictty where
  -- | Check if a key is in a dictionary.
  member :: dictty keyty elemty -> keyty -> Bool
  -- | Look up the data associated with a key in a 'Dict'.
  lookup :: dictty keyty elemty -> keyty -> Maybe elemty
  -- | Get all associations in this dictionary.
  assocs :: dictty keyty elemty -> [(keyty, elemty)]

  -- | Get all keys in this dictionary.
  keys :: dictty keyty elemty -> [keyty]
  keys = map fst . assocs
  -- | Get all elements in this dictionary.
  elems :: dictty keyty elemty -> [elemty]
  elems = map snd . assocs
