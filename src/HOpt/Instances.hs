module HOpt.Instances where

import HOpt.Datas

--------------------------------------------------------------------------------

instance Eq OptResult where
    or1 == or2 = (result or1) == (result or2)

--------------------------------------------------------------------------------

instance Ord OptResult where
    or1 `compare` or2 = (result or1) `compare` (result or2)
