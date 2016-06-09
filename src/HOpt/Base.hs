module HOpt.Base where

import HOpt.Types

--------------------------------------------------------------------------------

centerRange :: Range -> Double
centerRange range = 0.5 * ((snd range) - (fst range))

--------------------------------------------------------------------------------

maybeHead :: [a] -> Maybe a
maybeHead []  = Nothing
maybeHead (x:_) = Just x

--------------------------------------------------------------------------------

maybeTail :: [a] -> Maybe [a]
maybeTail []  = Nothing
maybeTail [_] = Nothing
maybeTail (_:xs) = Just xs

--------------------------------------------------------------------------------

maybeMinimum :: (Ord a) => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum x = Just $ minimum x
