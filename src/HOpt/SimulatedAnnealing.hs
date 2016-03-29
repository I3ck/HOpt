module HOpt.SimulatedAnnealing where

import HOpt.Types
import HOpt.Datas
import HOpt.Instances
import HOpt.Base

import System.Random

---TODO block defined both in H2d and hopt, maybe define in own project?
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

import Data.List.Split (chunksOf)
import Control.Parallel.Strategies (runEval, rpar, parMap)

--------------------------------------------------------------------------------

chunkParMap :: Int -> (a -> b) -> [a] -> [b]
chunkParMap chunkSize f l = concat $ parMap rpar (map f) (chunksOf chunkSize l)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
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

splitRange :: Int -> Range -> [Range]
splitRange nSplits r = zip minVals maxVals
  where
    minVals   = [(fst r)            , (fst r) + increment      , (snd r) - increment]
    maxVals   = [(fst r) + increment, (fst r) + 2.0 * increment, (snd r)]
    increment = ((snd r) - (fst r)) / fromIntegral nSplits

splitSingle :: SAMultiParams -> [SAparams] ---TODO might be best to directly create this list when creating the multi
splitSingle multi = map fromSingle (splitRanges multi)
  where
    fromSingle :: [Range] -> SAparams
    fromSingle rs = (saParams multi) { ranges = rs}

--------------------------------------------------------------------------------

simulAnnealMulti :: SAMultiParams -> OptResult
simulAnnealMulti multi = case (target $ saParams multi) of
    Minimize -> minimum $ chunkParMap (chunkSize multi) simulAnneal (splitSingle multi)
    Maximize -> maximum $ chunkParMap (chunkSize multi) simulAnneal (splitSingle multi)

--------------------------------------------------------------------------------

simulAnneal :: SAparams -> OptResult
simulAnneal params = simAnnealRecursion ((nCalculations params) - 1) randomNumbers initialResults (temperature params) (chance params) initialResult
  where
    initialResults    = [initialResult]
    initialResult     = (callback params) initialParams
    initialParams     = map centerRange (ranges params)
    randomNumbers     = randomRs (0.0 :: Double, 1.0 :: Double) (mkStdGen $ seed params)

    simAnnealRecursion :: NCalculations -> [Double] -> [OptResult] -> Double -> Double -> OptResult -> OptResult
    simAnnealRecursion nCalcLeft randNs previousResults temp chance prevBest
        | nCalcLeft <= 0 = newBest
        | otherwise      = simAnnealRecursion (nCalcLeft - 1) (tail randNs) newResults newTemp newChance newStart
      where
        lastResult = head previousResults
        newTemp    = (coolingFactor params) * temp
        newChance  = (coolingFactor params) * chance
        newResults = (newResult : previousResults)
        newStart   | (head randNs) < chance = lastResult
                   | otherwise = newBest
        newResult  = (callback params) newParams
        newParams  = calcNewParams (tail randNs) prevBest
        newBest    = case (target params) of
            Minimize -> min newResult prevBest
            Maximize -> max newResult prevBest

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
