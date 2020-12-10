{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = map (read . (: [])) . show $ x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherRev $ reverse xs

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x : y : xs) = x : y * 2 : doubleEveryOtherRev xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ toDigits $ read $ foldl (\acc curr -> show curr ++ acc) [] xs

validate :: Integer -> Bool
validate cc = 0 == (sumDigits . doubleEveryOther . toDigits) cc `rem` 10