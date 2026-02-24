module Main where

import Data.Array qualified as A
import Data.Char (isAscii, isLetter, toLower)
import Data.List (isPrefixOf)
import Data.Map.Lazy qualified as M
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type EntropyMap = M.Map String Double

defaultInputPath :: FilePath
defaultInputPath = "ishmael.dec"

defaultEntropyPath :: FilePath
defaultEntropyPath = "entropy.txt"

defaultOutputPath :: FilePath
defaultOutputPath = "ishmael.segmented.txt"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runSegment defaultInputPath defaultEntropyPath defaultOutputPath
    [inputPath, outputPath] -> runSegment inputPath defaultEntropyPath outputPath
    _ -> error "Usage: cabal run segment [input-file output-file]"

runSegment :: FilePath -> FilePath -> FilePath -> IO ()
runSegment inputPath entropyPath outputPath = do
  entropyMap <- loadEntropyMap entropyPath
  rawMessage <- readFile inputPath
  let message = normalizeMessage rawMessage
      segmentedWords = segmentMessage entropyMap message
      outputText = formatWords 60 segmentedWords
  writeFile outputPath outputText

-- Normalize text: keep lowercase ascii letters only.
normalizeMessage :: String -> String
normalizeMessage =
  map toLower . filter (\c -> isAscii c && isLetter c)

-- Load entropy word map from file.
loadEntropyMap :: FilePath -> IO EntropyMap
loadEntropyMap path = do
  contents <- readFile path
  let pairs = mapMaybe parseEntropyLine (lines contents)
  pure (M.fromAscList pairs)

-- Parse a line to extract (word, entropy) if valid.
parseEntropyLine :: String -> Maybe (String, Double)
parseEntropyLine line =
  case words line of
    [word, entropyStr] ->
      case readMaybe entropyStr of
        Just entropyVal -> Just (word, entropyVal)
        Nothing -> Nothing
    _ -> Nothing

-- Segment message using minimum total entropy, returns word list.
segmentMessage :: EntropyMap -> String -> [String]
segmentMessage _ [] = []
segmentMessage entropyMap message =
  let n = length message
      resultArr = buildResultArray entropyMap message
      wordArr :: A.Array Int String
      wordArr =
        A.array
          (0, n - 1)
          [(i, snd (resultArr A.! i)) | i <- [0 .. n - 1]]
   in recoverWords wordArr 0

-- DP: Table for minimal-entropy segmentation from each index.
buildResultArray :: EntropyMap -> String -> A.Array Int (Double, String)
buildResultArray entropyMap message =
  let n = length message
      baseArr =
        A.array (0, n) $
          [(i, (1 / 0, "")) | i <- [0 .. n - 1]]
            ++ [(n, (0.0, ""))]
   in fillFromRight baseArr (n - 1)
  where
    -- Helper: Fills DP table from right to left.
    fillFromRight :: A.Array Int (Double, String) -> Int -> A.Array Int (Double, String)
    fillFromRight resultArr i
      | i < 0 = resultArr
      | otherwise =
          let best = bestSplitAt entropyMap message resultArr i
              updatedArr = resultArr A.// [(i, best)]
           in fillFromRight updatedArr (i - 1)

-- Find the best scoring word starting at index 'i'.
bestSplitAt :: EntropyMap -> String -> A.Array Int (Double, String) -> Int -> (Double, String)
bestSplitAt entropyMap message resultArr i =
  let suffix = drop i message
      matchingPrefixes = prefixes entropyMap suffix
      candidates = map (scoreCandidate resultArr i) matchingPrefixes
   in case candidates of
        [] -> (1 / 0, "")
        _ -> minimumByScore ("index " ++ show i) candidates

-- Calculate score for a candidate word at position 'i' in message.
scoreCandidate :: A.Array Int (Double, String) -> Int -> (String, Double) -> (Double, String)
scoreCandidate resultArr i (word, wordEntropy) =
  let nextIndex = i + length word
      (restEntropy, _) = resultArr A.! nextIndex
      totalEntropy = wordEntropy + restEntropy
   in (totalEntropy, word)

-- Walks through word array to reconstruct words from start index.
recoverWords :: A.Array Int String -> Int -> [String]
recoverWords wordArr i
  | i > upper = []
  | otherwise =
      let word = wordArr A.! i
       in if null word
            then error ("recoverWords -- empty word at index " ++ show i)
            else word : recoverWords wordArr (i + length word)
  where
    (_, upper) = A.bounds wordArr

-- Retrieve all (word, entropy) pairs where word is a possible prefix at the start of message.
prefixes :: EntropyMap -> String -> [(String, Double)]
prefixes entropyMap message = go 1 []
  where
    limit = length message

    go :: Int -> [(String, Double)] -> [(String, Double)]
    go i acc
      | i > limit = reverse acc
      | otherwise =
          let pref = take i message
              (_, maybeEntropy, greaterThan) = M.splitLookup pref entropyMap
              acc' = case maybeEntropy of
                Just entropyVal -> (pref, entropyVal) : acc
                Nothing -> acc
              canContinue = case M.lookupMin greaterThan of
                Just (nextWord, _) -> pref `isPrefixOf` nextWord
                Nothing -> False
           in if canContinue
                then go (i + 1) acc'
                else reverse acc'

-- Given a list of (score, word), returns the one with the minimal score.
minimumByScore :: String -> [(Double, String)] -> (Double, String)
minimumByScore context [] =
  error ("minimumByScore -- no candidates at " ++ context)
minimumByScore _ [x] = x
minimumByScore context (x : xs) =
  let best = minimumByScore context xs
   in if fst x < fst best
        then x
        else best

-- Formats the words list into lines of at most the given width.
formatWords :: Int -> [String] -> String
formatWords lineWidth wordsList =
  unlines (buildLines "" wordsList)
  where
    buildLines :: String -> [String] -> [String]
    buildLines currentLine [] =
      if null currentLine then [] else [currentLine]
    buildLines "" (word : rest) =
      buildLines word rest
    buildLines currentLine (word : rest)
      | length currentLine + 1 + length word <= lineWidth =
          buildLines (currentLine ++ " " ++ word) rest
      | otherwise =
          currentLine : buildLines word rest
