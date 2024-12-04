data Date = Date { year :: Int, month :: Int, day :: Int }
  deriving (Eq)

instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2)
    | y1 /= y2  = compare y1 y2
    | m1 /= m2  = compare m1 m2
    | otherwise = compare d1 d2

instance Show Date where
  show (Date y m d) =
    let showWithLeadingZero n = if n < 10 then '0' : show n else show n
     in show y ++ "-" ++ showWithLeadingZero m ++ "-" ++ showWithLeadingZero d

instance Read Date where
  readsPrec _ input =
    case span (/= '-') input of
      (yStr, '-':rest1) -> case span (/= '-') rest1 of
        (mStr, '-':dStr) ->
          let y = read yStr
              m = read mStr
              d = read dStr
           in [(Date y m d, "")]
        _ -> []
      _ -> []

isValidDate :: Date -> Bool
isValidDate (Date y m d) =
  let daysInMonth y m
        | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
        | m `elem` [4, 6, 9, 11]           = 30
        | m == 2                           = if isLeapYear y then 29 else 28
        | otherwise                        = 0
      isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)
   in m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth y m

addDays :: Int -> Date -> Date
addDays n date@(Date y m d)
  | n == 0 = date
  | n > 0  = addDays (n - 1) (nextDay date)
  | n < 0  = addDays (n + 1) (prevDay date)

subtractDates :: Date -> Date -> Int
subtractDates date1 date2 = go date1 date2 0
  where
    go d1 d2 acc
      | d1 == d2  = acc
      | d1 < d2   = go (nextDay d1) d2 (acc - 1)
      | otherwise = go (prevDay d1) d2 (acc + 1)
      
nextDay :: Date -> Date
nextDay (Date y m d)
  | d < daysInMonth y m = Date y m (d + 1)
  | m < 12              = Date y (m + 1) 1
  | otherwise           = Date (y + 1) 1 1
  where
    daysInMonth y m
      | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
      | m `elem` [4, 6, 9, 11]           = 30
      | m == 2                           = if isLeapYear y then 29 else 28
      | otherwise                        = 0
    isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

prevDay :: Date -> Date
prevDay (Date y m d)
  | d > 1     = Date y m (d - 1)
  | m > 1     = Date y (m - 1) (daysInMonth y (m - 1))
  | otherwise = Date (y - 1) 12 31
  where
    daysInMonth y m
      | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
      | m `elem` [4, 6, 9, 11]           = 30
      | m == 2                           = if isLeapYear y then 29 else 28
      | otherwise                        = 0
    isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)
