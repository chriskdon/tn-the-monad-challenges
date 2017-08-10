{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ (show a)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay key ((lk, lv):xs)
  | key == lk = Just lv
  | otherwise = lookupMay key xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:xs) = Just $ foldl (\a b -> if a > b then a else b) x xs 

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:xs) = Just $ foldl (\a b -> if a < b then a else b) x xs 

-- Ugly --> will be converted to use monads later
queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = 
  case lookupMay key gd of
      Nothing -> Nothing
      Just xs -> case tailMay xs of
          Nothing -> Nothing
          Just tail -> case maximumMay tail of
              Nothing -> Nothing
              Just max -> case headMay xs of
                  Nothing -> Nothing
                  Just head -> divMay (fromIntegral max) (fromIntegral head)