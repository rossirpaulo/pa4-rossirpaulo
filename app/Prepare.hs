module Main where

import Data.Char (isAscii, isLetter, toLower)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Text.Read (readMaybe)

inputPath :: FilePath
inputPath = "enwiki-2023-04-13.txt"

outputPath :: FilePath
outputPath = "entropy.txt"

minOccurrences :: Int
minOccurrences = 50

type CountMap = M.Map String Int

main :: IO ()
main = do
  corpus <- readFile inputPath
  let counts = buildCountMap corpus
      filtered = M.filter (>= minOccurrences) counts
      entropies = computeEntropies filtered
      output = unlines (map renderEntropyLine (M.toAscList entropies))
  writeFile outputPath output

buildCountMap :: String -> CountMap
buildCountMap corpus =
  let lineMaps = map processLine (lines corpus)
   in M.unionsWith (+) lineMaps

processLine :: String -> CountMap
processLine line =
  case words line of
    [rawWord, nStr]
      | all isAscii rawWord ->
            Just n
              | n > 0 ->
                  let subwords = splitOn "-" rawWord
                      cleaned = map normalizeSubword subwords
                      validWords = filter (not . null) cleaned
                   in M.fromListWith (+) [(w, n) | w <- validWords]
               in M.fromListWith (+) [(w, n) | w <- validWords]
            _ -> M.empty
    _ -> M.empty

normalizeSubword :: String -> String
normalizeSubword =
  map toLower . filter isLetter

computeEntropies :: CountMap -> M.Map String Double
computeEntropies counts =
  let total = fromIntegral (sum (M.elems counts)) :: Double
      wordEntropy n = negate (logBase 2 (fromIntegral n / total))
   in M.map wordEntropy counts

renderEntropyLine :: (String, Double) -> String
renderEntropyLine (w, e) = w ++ " " ++ show e
