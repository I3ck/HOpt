module HOpt.Base where

import HOpt.Types

--------------------------------------------------------------------------------

centerRange :: Range -> Double
centerRange range = 0.5 * ((snd range) - (fst range))
