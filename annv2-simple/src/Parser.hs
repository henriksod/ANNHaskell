module Parser
( readFileToList
, parse
, resultFile
, getImageNameList
, getDataLength
, prepareInput
, prepareOutput
, sampleImage
) where

import Text.Regex
import Text.Regex.Posix
import Data.List
import Text.Read
import Data.List.Split
import Data.Maybe
import Data.Matrix (Matrix, (<|>), (<->))
import qualified Data.Matrix as M
import qualified Data.Vector as V

import ANN.MatrixUtils

imageNameRegex :: String
imageNameRegex = "(Image([[:graph:]]+))"

imageFacitRegex :: String
imageFacitRegex = "(Image([[:graph:]]+)[[:blank:]]([[:graph:]]+))"

imageContentRegex :: String
imageContentRegex = "([[:digit:]]+)([[:blank:]]*[[:digit:]]+)*"

readFileToList :: String -> IO [String]
readFileToList s = lines <$> readFile s

parse :: [String] -> [(Int, [Double])]
parse [] = []
parse ls = reverse $ parseData ls []

parseData :: [String] -> [(Int, [Double])] -> [(Int, [Double])]
parseData [] ds = ds
parseData (l:ls) []
  | isJust (getNextFacit (regex l imageFacitRegex)) =
      parseData ls
        [(fromJust (getNextImage (regex l imageNameRegex)),
          [fromJust (getNextFacit (regex l imageFacitRegex))])]
  | otherwise =
    case getNextImage (regex l imageNameRegex) of
            Just i -> parseData ls [(i,[])]
            Nothing -> parseData ls []
parseData (l:ls) ((id,list):ds)
  | matchIsNull (regex l imageNameRegex) (regex l imageContentRegex)
      || ("#" `isPrefixOf` l)
    = parseData ls ((id,list):ds)
  | isJust (getNextFacit (regex l imageFacitRegex)) =
      parseData ls
        ((fromJust (getNextImage (regex l imageNameRegex)),
          [fromJust (getNextFacit (regex l imageFacitRegex))])
            : [] ++ ((id,list):ds))
  | otherwise =
    case getNextImage (regex l imageNameRegex) of
      Just i -> parseData ls ((i,[]):[] ++ ((id,list):ds))
      Nothing -> case getNextDataLine (regex l imageContentRegex) of
                  Just d -> parseData ls ((id,list ++ d):ds)
                  Nothing -> parseData ls ((id,list):ds)

regex :: String -> String -> MatchResult String
regex f p = f =~ p

matchIsNull :: MatchResult String -> MatchResult String -> Bool
matchIsNull (MR _ _ _ l _) (MR _ _ _ j _) = null l && null j

getNextImage :: MatchResult String -> Maybe Int
getNextImage (MR _ _ _ [] _) = Nothing
getNextImage (MR _ _ _ (l:ls) _)
  | "Image" `isPrefixOf` l = readMaybe (head ls) :: Maybe Int
  | otherwise = Nothing

getNextFacit :: MatchResult String -> Maybe Double
getNextFacit (MR _ _ _ [] _) = Nothing
getNextFacit (MR _ _ _ ls _)
  | length ls == 3 = readMaybe (ls!!2) :: Maybe Double
  | otherwise = Nothing

getNextDataLine :: MatchResult String -> Maybe [Double]
getNextDataLine (MR _ _ _ [] _) = Nothing
getNextDataLine (MR _ str _ (l:_) _) =
  case (readMaybe l :: Maybe Double) of
    Just i -> Just (map (/32.0) (map read $ splitOn " " str :: [Double]))
    Nothing -> Nothing

resultFile :: [Int] -> [Int] -> [String]
resultFile [] _ = []
resultFile _ [] = []
resultFile (r:rs) (i:is) = ("Image" ++ show i ++ " " ++ show r)
                           : resultFile rs is

getImageNameList :: [(Int, [Double])] -> [Int]
getImageNameList [] = []
getImageNameList ((i,_):ls) = i : getImageNameList ls

getDataLength :: [(Int, [Double])] -> Int
getDataLength ((_,d):_) = length d

prepareInput :: [(Int, [Double])] -> [Matrix Double]
prepareInput [] = []
prepareInput ((_,d):ls) =
  M.colVector (V.fromList d) : prepareInput ls

prepareOutput :: [(Int, [Double])] -> [Matrix Double]
prepareOutput [] = []
prepareOutput ((_,d:_):ls) =
  M.colVector (V.fromList (outputToList (round d) 4)) : prepareOutput ls

outputToList :: Int -> Int -> [Double]
outputToList o n
  | n == 1 = if n == o then [1.0] else [0.0]
  | n > 1 = outputToList o (n-1) ++ (if n == o then [1.0] else [0.0])
  | otherwise = []

-- Sampling
hM1 :: Matrix Double
hM1 = M.fromLists [ [1,1]
                   ,[0,0] ]
hM2 :: Matrix Double
hM2 = M.fromLists [ [0,0]
                   ,[1,1] ]
vM1 :: Matrix Double
vM1 = M.fromLists [ [1,0]
                   ,[1,0] ]
vM2 :: Matrix Double
vM2 = M.fromLists [ [0,1]
                   ,[0,1] ]
dM1 :: Matrix Double
dM1 = M.fromLists [ [0,1]
                   ,[1,0] ]
dM2 :: Matrix Double
dM2 = M.fromLists [ [1,0]
                   ,[0,1] ]

sampleImage :: Int -> Int -> [Double] -> [Double]
sampleImage x sz ls
  | x > 0 = sampleImage (x-1) newSz (divAll sample (maximum absSample))
  | otherwise = ls
  where img = M.fromList sz sz ls
        ftrMs = fmap (rep newSz) [hM1,hM2,vM1,vM2,dM1,dM2]
        rep s m = repV s (repH s m)
        repH s m = foldl1 (<|>) (replicate s m) :: Matrix Double
        repV s m = foldl1 (<->) (replicate s m) :: Matrix Double
        sample = map (^2) (sumAll2x2 (M.fromList newSz newSz prepSample))
        absSample = map abs sample
        prepSample = foldl1 (zipWith (+)) (map (M.toList . hadamard img) ftrMs)
        newSz = quot sz 2

divAll :: [Double] -> Double -> [Double]
divAll [] _ = []
divAll (l:ls) d = l / d : divAll ls d

keepZeroLogBase32 :: Double -> Double
keepZeroLogBase32 a
  | a == 0 = a
  | otherwise = logBase 32.0 a
