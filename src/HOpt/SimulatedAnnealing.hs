module HOpt.SimulatedAnnealing where

import HOpt.Types
import HOpt.Datas
import HOpt.Instances
import HOpt.Base

import Data.List
import Data.Maybe
import System.Random
import Data.List.Split (chunksOf)
import Control.Parallel.Strategies (runEval, rpar, parMap)

--------------------------------------------------------------------------------

chunkParMap :: Int -> (a -> b) -> [a] -> [b]
chunkParMap chunkSize f l = concat $ parMap rpar (map f) (chunksOf chunkSize l)

--------------------------------------------------------------------------------

makeMulti :: SAparams -> Int -> Int -> SAMultiParams ---TODO reorder
makeMulti saParams chunkSize nSplits = saMultiParams
  where
    saMultiParams = SAMultiParams
        { saParams = saParams
        , splitRanges = newRanges
        , chunkSize = chunkSize
        }

    newRanges = map (splitRange nSplits) (ranges saParams)

--------------------------------------------------------------------------------

splitRange :: Int -> Range -> [Range]
splitRange nSplits r = zip minVals maxVals
  where
    minVals   = [(fst r)            , (fst r) + increment      , (snd r) - increment]
    maxVals   = [(fst r) + increment, (fst r) + 2.0 * increment, (snd r)]
    increment = ((snd r) - (fst r)) / fromIntegral nSplits --   TODO nSplits - 1 ?

--------------------------------------------------------------------------------

splitSingle :: SAMultiParams -> [SAparams] ---TODO might be best to directly create this list when creating the multi
splitSingle multi = map fromSingle transposed
  where
    transposed = transpose (splitRanges multi)

    fromSingle :: [Range] -> SAparams
    fromSingle rs = (saParams multi) { ranges = rs}

--------------------------------------------------------------------------------

simulAnnealMulti :: SAMultiParams -> Maybe OptResult
simulAnnealMulti multi = maybeMinimum $ catMaybes $ chunkParMap (chunkSize multi) simulAnneal (splitSingle multi)

--------------------------------------------------------------------------------

simulAnneal :: SAparams -> Maybe OptResult
simulAnneal params = simAnnealRecursion ((nCalculations params) - 1) randomNumbers initialResults (temperature params) (chance params) initialResult
  where
    initialResults    = [initialResult]
    initialResult     = (callback params) initialParams
    initialParams     = map centerRange (ranges params)
    randomNumbers     = randomRs (0.0 :: Double, 1.0 :: Double) (mkStdGen $ seed params)

    simAnnealRecursion :: NCalculations -> [Double] -> [OptResult] -> Double -> Double -> OptResult -> Maybe OptResult
    simAnnealRecursion nCalcLeft randNs previousResults temp chance prevBest = case (maybeTail randNs, maybeHead previousResults, maybeHead randNs) of
        (Just tailRndms, Just lastResult, Just rand)
            | nCalcLeft <= 0 -> Just newBest
            | otherwise      ->  simAnnealRecursion (nCalcLeft - 1) tailRndms newResults newTemp newChance newStart
                  where
                    newTemp    = (coolingFactor params) * temp
                    newChance  = (coolingFactor params) * chance
                    newResults = (newResult : previousResults)
                    newStart   | rand < chance = lastResult
                               | otherwise = newBest
                    newResult  = (callback params) newParams
                    newParams  = calcNewParams tailRndms prevBest
                    newBest    = min newResult prevBest

                    calcNewParams :: [Double] -> OptResult -> OptParams
                    calcNewParams randNs center = newParams
                      where
                        newParams = map randomValidParam $ zip3 (ranges params) (inputs center) randNs

                        randomValidParam (range, center, random) = randomParam
                          where
                            randomParam   = minTotal + random * delta
                            delta         = maxTotal - minTotal
                            maxTotal      = min maxR maxC
                            minTotal      = max minR minC
                            maxR          = snd range
                            minR          = fst range
                            maxC          = center + maxHalfLength
                            minC          = center - maxHalfLength
                            maxHalfLength = temp * centerRange range
        _ -> Nothing
