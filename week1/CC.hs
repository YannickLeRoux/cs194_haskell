{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits = map (read . (: [])) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : xs) = aux (x : xs) 1
  where
    aux l i = (x * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits = sum

-- validate :: Integer -> Bool