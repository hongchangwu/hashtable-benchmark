module Main (main) where

import           Criterion.Main
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HashMap
import           Data.HashTable.Class (HashTable)
import           Data.HashTable.IO    (BasicHashTable, CuckooHashTable,
                                       IOHashTable, LinearHashTable)
import qualified Data.HashTable.IO    as IOHashTable
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.List            (foldl')
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Test.QuickCheck      (generate, vector)

size :: Int
size = 1000000

type InsertInput = ([Int], [Int])

generateInsertInput :: Int -> IO InsertInput
generateInsertInput k = (,) <$> return [1 .. k] <*> generate (vector k)

type LookupInput = [Int]

generateLookupInput :: Int -> IO LookupInput
generateLookupInput k = generate (vector k)

main :: IO ()
main =
  defaultMain
    [ env (generateInsertInput size) $
      \ ~(sorted, random) ->
         bgroup
           "insert"
           [ bgroup
               "Data.Map"
               [ bench "sorted" $ whnf insertMap sorted
               , bench "random" $ whnf insertMap random
               ]
           , bgroup
               "Data.IntMap"
               [ bench "sorted" $ whnf insertIntMap sorted
               , bench "random" $ whnf insertIntMap random
               ]
           , bgroup
               "Data.HashMap.Lazy"
               [ bench "sorted" $ whnf insertHashMap sorted
               , bench "random" $ whnf insertHashMap random
               ]
           , bgroup
               "Data.HashTable.ST.Basic"
               [ bench "sorted" $
                 whnfIO
                   (insertHashTableIO sorted :: IO (BasicHashTable Int Int))
               , bench "random" $
                 whnfIO
                   (insertHashTableIO random :: IO (BasicHashTable Int Int))
               ]
           , bgroup
               "Data.HashTable.ST.Cuckoo"
               [ bench "sorted" $
                 whnfIO
                   (insertHashTableIO sorted :: IO (CuckooHashTable Int Int))
               , bench "random" $
                 whnfIO
                   (insertHashTableIO random :: IO (CuckooHashTable Int Int))
               ]
           , bgroup
               "Data.HashTable.ST.Linear"
               [ bench "sorted" $
                 whnfIO
                   (insertHashTableIO sorted :: IO (LinearHashTable Int Int))
               , bench "random" $
                 whnfIO
                   (insertHashTableIO random :: IO (LinearHashTable Int Int))
               ]
           ]
    , env (generateLookupInput size) $
      \ks ->
         bgroup
           "lookup"
           [ bench "Data.Map" $ nf lookupMap ks
           , bench "Data.IntMap" $ nf lookupIntMap ks
           , bench "Data.HashMap.Lazy" $ nf lookupHashMap ks
           ]
    ]

insertMap :: [Int] -> Map Int Int
insertMap = foldl' (\m k -> Map.insert k 1 m) Map.empty

insertIntMap :: [Int] -> IntMap Int
insertIntMap = foldl' (\m k -> IntMap.insert k 1 m) IntMap.empty

insertHashMap :: [Int] -> HashMap Int Int
insertHashMap = foldl' (\m k -> HashMap.insert k 1 m) HashMap.empty

insertHashTableIO
  :: HashTable h
  => [Int] -> IO (IOHashTable h Int Int)
insertHashTableIO xs = do
  ht <- IOHashTable.new
  mapM_ (\k -> IOHashTable.insert ht k 1) xs
  return ht

aMap :: Map Int Int
aMap = Map.fromAscList [(k, 1) | k <- [1 .. size]]

lookupMap :: [Int] -> [Maybe Int]
lookupMap = map (`Map.lookup` aMap)

aIntMap :: IntMap Int
aIntMap = IntMap.fromAscList [(k, 1) | k <- [1 .. size]]

lookupIntMap :: [Int] -> [Maybe Int]
lookupIntMap = map (`IntMap.lookup` aIntMap)

aHashMap :: HashMap Int Int
aHashMap = HashMap.fromList [(k, 1) | k <- [1 .. size]]

lookupHashMap :: [Int] -> [Maybe Int]
lookupHashMap = map (`HashMap.lookup` aHashMap)
