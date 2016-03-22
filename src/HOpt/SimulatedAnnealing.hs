module HOpt.SimulatedAnnealing where

import HOpt.Types
import HOpt.Datas
import HOpt.Instances
import HOpt.Base

import System.Random

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
