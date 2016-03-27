module HOpt.Datas where

import HOpt.Types

--------------------------------------------------------------------------------

data SAparams = SAparams
    { coolingFactor :: Double
    , temperature   :: Double
    , chance        :: Double
    , ranges        :: [Range]
    , seed          :: Int
    , target        :: Target
    , nCalculations :: Int
    , callback      :: OptParams -> OptResult
    }

data SAMultiParams = SAMultiParams
    { saParams :: SAparams
    , splitRanges :: [[Range]]
    }

--------------------------------------------------------------------------------

data OptResult = OptResult
    { inputs :: OptParams
    , result :: Double
    } deriving (Show)

--------------------------------------------------------------------------------

data Target = Minimize
            | Maximize
