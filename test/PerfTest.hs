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

module Main(main) where

import Control.Monad
import Criterion.Main
import Data.Dict.FKS(FKSDict)
import Data.IntMap(IntMap)
import System.IO
import System.Random

import qualified Data.Set as Set
import qualified Data.Dict.FKS as FKS
import qualified Data.IntMap.Lazy as IntMap

createEntries :: Int -> Int -> IO [(Int, Int)]
createEntries max num =
  let
    genrandom :: IO Int
    genrandom =
      do
        n <- randomIO
        return (n `mod` max)

    filterfun :: [Int] -> [Int]
    filterfun l = Set.toList $! Set.fromList l
  in do
    rand <- forM [0..num] (const genrandom)
    return $! zip (filterfun rand) [0..num]

createAccesses :: Int -> Int -> Int -> IO (IntMap Int, FKSDict Int Int, [Int])
createAccesses max nentries naccesses =
  let
    genrandom :: IO Int
    genrandom =
      do
        n <- randomIO
        return (n `mod` max)
  in do
    accesses <- forM [0..naccesses] (const genrandom)
    ents <- createEntries max nentries
    d <- FKS.dict ents
    return (IntMap.fromList ents, d, accesses)

dictCreationBenchmark :: [(Int, Int)] -> Benchmark
dictCreationBenchmark = bench "FKSDict" . whnfIO . FKS.dict

intmapCreationBenchmark :: [(Int, Int)] -> Benchmark
intmapCreationBenchmark = bench "IntMap" . whnf IntMap.fromList

creationBenchmark :: String -> [(Int, Int)] -> Benchmark
creationBenchmark name elems = bgroup name [
    intmapCreationBenchmark elems,
    dictCreationBenchmark elems
  ]

dictLookupBenchmark :: FKSDict Int Int -> [Int] -> Benchmark
dictLookupBenchmark d = bench "FKSDict" . nf (map (FKS.lookup d))

intmapLookupBenchmark :: IntMap Int -> [Int] -> Benchmark
intmapLookupBenchmark m = bench "IntMap" . nf (map (\i -> IntMap.lookup i m))

dictMemberBenchmark :: FKSDict Int Int -> [Int] -> Benchmark
dictMemberBenchmark d = bench "FKSDict" . nf (map (FKS.member d))

intmapMemberBenchmark :: IntMap Int -> [Int] -> Benchmark
intmapMemberBenchmark m = bench "IntMap" . nf (map (\i -> IntMap.member i m))

lookupBenchmark :: (IntMap Int, FKSDict Int Int, [Int]) -> Benchmark
lookupBenchmark ~(i, d, accesses) = bgroup "lookup" [
    dictLookupBenchmark d accesses,
    intmapLookupBenchmark i accesses
  ]

memberBenchmark :: (IntMap Int, FKSDict Int Int, [Int]) -> Benchmark
memberBenchmark ~(i, d, accesses) = bgroup "member" [
    dictMemberBenchmark d accesses,
    intmapMemberBenchmark i accesses
  ]

accessBenchmark :: String -> (IntMap Int, FKSDict Int Int, [Int]) -> Benchmark
accessBenchmark name dat = bgroup name [
    memberBenchmark dat,
    lookupBenchmark dat
  ]

main :: IO ()
main = defaultMain [
  {-
    bgroup "create" [
      env (createEntries 4 4) (creationBenchmark "4_4"),
      env (createEntries 4 16) (creationBenchmark "4_16"),
      env (createEntries 4 64) (creationBenchmark "4_64"),
      env (createEntries 4 1024) (creationBenchmark "4_1024"),
      env (createEntries 4 16384) (creationBenchmark "4_16384"),
      env (createEntries 16 16) (creationBenchmark "16_16"),
      env (createEntries 16 64) (creationBenchmark "16_64"),
      env (createEntries 16 256) (creationBenchmark "16_256"),
      env (createEntries 16 4096) (creationBenchmark "16_4096"),
      env (createEntries 16 65536) (creationBenchmark "16_65536"),
      env (createEntries 64 64) (creationBenchmark "64_64"),
      env (createEntries 64 256) (creationBenchmark "64_256"),
      env (createEntries 64 1024) (creationBenchmark "64_1024"),
      env (createEntries 64 16384) (creationBenchmark "64_16384"),
      env (createEntries 64 262144) (creationBenchmark "64_262144"),
      env (createEntries 256 256) (creationBenchmark "256_256"),
      env (createEntries 256 1024) (creationBenchmark "256_1024"),
      env (createEntries 256 4096) (creationBenchmark "256_4096"),
      env (createEntries 256 65536) (creationBenchmark "256_65536"),
      env (createEntries 256 1048576) (creationBenchmark "256_1048576"),
      env (createEntries 16384 16384) (creationBenchmark "16384_16384"),
      env (createEntries 16384 65536) (creationBenchmark "16384_65536"),
      env (createEntries 16384 262144) (creationBenchmark "16384_262144"),
      env (createEntries 16384 4194304) (creationBenchmark "16384_4194304"),
      env (createEntries 16384 16777216) (creationBenchmark "16384_16777216")
    ],
-}
    bgroup "access" [
      env (createAccesses 4 4 10) (accessBenchmark "4_4_10"),
      env (createAccesses 4 4 100) (accessBenchmark "4_4_100"),
      env (createAccesses 4 4 1000) (accessBenchmark "4_4_1000"),
      env (createAccesses 4 16 10) (accessBenchmark "4_16_10"),
      env (createAccesses 4 16 100) (accessBenchmark "4_16_100"),
      env (createAccesses 4 16 1000) (accessBenchmark "4_16_1000"),
      env (createAccesses 4 64 10) (accessBenchmark "4_64_10"),
      env (createAccesses 4 64 100) (accessBenchmark "4_64_100"),
      env (createAccesses 4 64 1000) (accessBenchmark "4_64_1000"),
      env (createAccesses 4 1024 10) (accessBenchmark "4_1024_10"),
      env (createAccesses 4 1024 100) (accessBenchmark "4_1024_100"),
      env (createAccesses 4 1024 1000) (accessBenchmark "4_1024_1000"),
      env (createAccesses 4 16384 10) (accessBenchmark "4_16384_10"),
      env (createAccesses 4 16384 100) (accessBenchmark "4_16384_100"),
      env (createAccesses 4 16384 1000) (accessBenchmark "4_16384_1000"),
      env (createAccesses 16 16 10) (accessBenchmark "16_16_10"),
      env (createAccesses 16 16 100) (accessBenchmark "16_16_100"),
      env (createAccesses 16 16 1000) (accessBenchmark "16_16_1000"),
      env (createAccesses 16 64 10) (accessBenchmark "16_64_10"),
      env (createAccesses 16 64 100) (accessBenchmark "16_64_100"),
      env (createAccesses 16 64 1000) (accessBenchmark "16_64_1000"),
      env (createAccesses 16 256 10) (accessBenchmark "16_256_10"),
      env (createAccesses 16 256 100) (accessBenchmark "16_256_100"),
      env (createAccesses 16 256 1000) (accessBenchmark "16_256_1000"),
      env (createAccesses 16 4096 10) (accessBenchmark "16_4096_10"),
      env (createAccesses 16 4096 100) (accessBenchmark "16_4096_100"),
      env (createAccesses 16 4096 1000) (accessBenchmark "16_4096_1000"),
      env (createAccesses 16 65536 10) (accessBenchmark "16_65536_10"),
      env (createAccesses 16 65536 100) (accessBenchmark "16_65536_100"),
      env (createAccesses 16 65536 1000) (accessBenchmark "16_65536_1000"),
      env (createAccesses 64 64 10) (accessBenchmark "64_64_10"),
      env (createAccesses 64 64 100) (accessBenchmark "64_64_100"),
      env (createAccesses 64 64 1000) (accessBenchmark "64_64_1000"),
      env (createAccesses 64 256 10) (accessBenchmark "64_256_10"),
      env (createAccesses 64 256 100) (accessBenchmark "64_256_100"),
      env (createAccesses 64 256 1000) (accessBenchmark "64_256_1000"),
      env (createAccesses 64 1024 10) (accessBenchmark "64_1024_10"),
      env (createAccesses 64 1024 100) (accessBenchmark "64_1024_100"),
      env (createAccesses 64 1024 1000) (accessBenchmark "64_1024_1000"),
      env (createAccesses 64 16384 10) (accessBenchmark "64_16384_10"),
      env (createAccesses 64 16384 100) (accessBenchmark "64_16384_100"),
      env (createAccesses 64 16384 1000) (accessBenchmark "64_16384_1000"),
      env (createAccesses 64 262144 10) (accessBenchmark "64_262144_10"),
      env (createAccesses 64 262144 100) (accessBenchmark "64_262144_100"),
      env (createAccesses 64 262144 1000) (accessBenchmark "64_262144_1000"),
      env (createAccesses 256 256 10) (accessBenchmark "256_256_10"),
      env (createAccesses 256 256 100) (accessBenchmark "256_256_100"),
      env (createAccesses 256 256 1000) (accessBenchmark "256_256_1000"),
      env (createAccesses 256 1024 10) (accessBenchmark "256_1024_10"),
      env (createAccesses 256 1024 100) (accessBenchmark "256_1024_100"),
      env (createAccesses 256 1024 1000) (accessBenchmark "256_1024_1000"),
      env (createAccesses 256 4096 10) (accessBenchmark "256_4096_10"),
      env (createAccesses 256 4096 100) (accessBenchmark "256_4096_100"),
      env (createAccesses 256 4096 1000) (accessBenchmark "256_4096_1000"),
      env (createAccesses 256 65536 10) (accessBenchmark "256_65536_10"),
      env (createAccesses 256 65536 100) (accessBenchmark "256_65536_100"),
      env (createAccesses 256 65536 1000) (accessBenchmark "256_65536_1000"),
      env (createAccesses 256 1048576 10) (accessBenchmark "256_1048576_10"),
      env (createAccesses 256 1048576 100) (accessBenchmark "256_1048576_100"),
      env (createAccesses 256 1048576 1000)
          (accessBenchmark "256_1048576_1000"),
      env (createAccesses 16384 16384 10) (accessBenchmark "16384_16384_10"),
      env (createAccesses 16384 16384 100) (accessBenchmark "16384_16384_100"),
      env (createAccesses 16384 16384 1000)
          (accessBenchmark "16384_16384_1000"),
      env (createAccesses 16384 65536 10) (accessBenchmark "16384_65536_10"),
      env (createAccesses 16384 65536 100) (accessBenchmark "16384_65536_100"),
      env (createAccesses 16384 65536 1000)
          (accessBenchmark "16384_65536_1000"),
      env (createAccesses 16384 262144 10) (accessBenchmark "16384_262144_10"),
      env (createAccesses 16384 262144 100)
          (accessBenchmark "16384_262144_100"),
      env (createAccesses 16384 262144 1000)
          (accessBenchmark "16384_262144_1000"),
      env (createAccesses 16384 4194304 10)
          (accessBenchmark "16384_4194304_10"),
      env (createAccesses 16384 4194304 100)
          (accessBenchmark "16384_4194304_100"),
      env (createAccesses 16384 4194304 1000)
          (accessBenchmark "16384_4194304_1000"),
      env (createAccesses 16384 16777216 10)
          (accessBenchmark "16384_16777216_10"),
      env (createAccesses 16384 16777216 100)
          (accessBenchmark "16384_16777216_100"),
      env (createAccesses 16384 16777216 1000)
          (accessBenchmark "16384_16777216_1000")
    ]
  ]
