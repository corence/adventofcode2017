
module Dates where

-- https://puzzling.stackexchange.com/questions/59332/asking-for-a-date

dates :: [String]
dates = do
  year <- [1973..2068]
  month <- [1..12]
  day <- [1..31]
  pure ()
