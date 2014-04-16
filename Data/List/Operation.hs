module Data.List.Operation where

import Data.List

getFirstPairWhenSumIs10 :: [Integer] -> Maybe (Int, Int)
getFirstPairWhenSumIs10 = getPairIndexWith (\x y -> x + y == 10) (\x -> x < 10)

-- Function to get the index in a list of the 2 first elements which comply
-- with a test. It is also possible to apply a filter to avoid other load.
getPairIndexWith :: (Eq a) => (a -> a -> Bool) -- a test function
                           -> (a -> Bool)      -- a filter function (could be (\_ -> True)
                           -> [a]              -- a list to look in
                           -> Maybe (Int, Int)
getPairIndexWith _       _         []   = Nothing
getPairIndexWith funTest funFilter [a]  = Nothing
getPairIndexWith funTest funFilter list =
    getPairFoldl list 0 []
    where
        getPairFoldl []     _     _ = Nothing
        getPairFoldl (x:xs) index acc
            | funFilter x =
                case find (\(_, t) -> funTest x t) acc of
                    Just (indice, _) -> Just (indice, index)
                    Nothing          ->
                        case find (\(_, s) -> s == x) acc of
                            Nothing -> getPairFoldl xs (index+1) ((index, x):acc)
                            Just _  -> getPairFoldl xs (index+1) acc
            | otherwise = getPairFoldl xs (index+1) acc
