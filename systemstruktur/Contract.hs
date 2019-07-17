module Contract where

data Date = Date String
    deriving (Ord, Eq, Show)

data Currency = EUR | GBP
  deriving (Eq, Show)

data Contract =
    Zero
  | One Currency
  | Scale Double Contract
  | When Date Contract
  | And Contract Contract
  | Give Contract
  deriving (Eq, Show)

-- Zero Coupon Bond
-- In der Zukunft
-- 100 Stück
-- Währung
zcb :: Date -> Double -> Currency -> Contract
zcb date amount currency = 
    When date (Scale amount (One currency))

date = Date

-- "Zahle mir 100 GBP am 1.10.2019"
c1 = zcb (date "2019-10-01") 100 GBP
c2 = zcb (date "2020-10-01") 100 EUR
c3 = And c1 c2

data Payout = Payout Date Double Currency

-- Residualvertrag ausrechnen + Zahlungen + nächstes Aufwachdatum
-- step :: Contract -> Date -> ([Payout], Contract, Date)
