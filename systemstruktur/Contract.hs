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
c4 = Scale 100 (And c1 c2)

data Payout = Payout Date Double Currency

-- Zahlung skalieren
scalePayout :: Double -> Payout -> Payout
scalePayout factor (Payout date amount currency) =
  Payout date (factor * amount) currency

-- das kleinere Maybe Datum ausrechnen
minDate :: Maybe Date -> Maybe Date -> Maybe Date
--minDate date1 date2 = min date1 date2 -- falsches Ord
minDate Nothing Nothing     = Nothing
minDate Nothing (Just d2)   = Just d2
minDate (Just d1) Nothing   = Just d1
minDate (Just d1) (Just d2) = Just (min d1 d2)
-- minDate md1 md2 = minimum (catMaybes [md1, md2])

-- "smart constructor"
scale :: Double -> Contract -> Contract
scale factor Zero = Zero
scale factor contract = Scale factor contract

-- Residualvertrag ausrechnen + Zahlungen + nächstes Aufwachdatum
step :: Contract -> Date -> ([Payout], Contract, Maybe Date)
step Zero date = ([], Zero, Nothing)
step (One currency) date = ([Payout date 1 currency], Zero, Nothing)
step (Scale factor contract) date =
  let (payouts, contract', date') = step contract date
  in (map (scalePayout factor) payouts, scale factor contract', date')
step contract@(When contractDate contract') date = 
  if contractDate <= date
  then step contract' date
  else ([], contract, Just contractDate)
step (And contract1 contract2) date =
  let (payouts1, contract1', date1) = step contract1 date
      (payouts2, contract2', date2) = step contract2 date
  in (payouts1 ++ payouts2, And contract1' contract2', minDate date1 date2)
step (Give contract) date = 
  let (payouts, contract', date') = step contract date
  in (map (scalePayout (-1)) payouts, Give contract', date')