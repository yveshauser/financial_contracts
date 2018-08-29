import Prelude hiding (and, or)
import Data.Aeson
import qualified Data.ByteString.Lazy as L

import Contracts
import Derivatives

-- dc example, p.196
-- value(dc_example, t0) = 98 - 4 + 1 = 95
dc_example :: Contract
dc_example = with_costs 1 $ dc term strike underlying
  where
    costs = 1
    term = 125.00 -- 0.5y
    strike = 105
    underlying = amount 100

-- bdc example, p.209
-- value(bdc_example, t0) = 105 - 16 + 6 + 2 = 97
bdc_example :: Contract
bdc_example = with_costs 2 $ bdc term strike barrier underlying
  where
    costs = 1
    term = 125.00 -- 0.5y
    strike = 105
    underlying = amount 105
    barrier = 80

-- rc example, p.218
-- ZBC + Coupon: 970 + 116.5
-- 10 x Short Put: -110
-- Indirect Costs: 23.5
-- value(rc_example, t0) == 10000
rc_example :: Contract
rc_example = with_costs costs $ rc term nominal coupon ratio strike underlying
  where
    costs = 23.5
    term = 250.00 -- 1y
    nominal = 1000
    coupon = 0.12
    strike = 100
    ratio = 10
    underlying = times ratio $ amount 100

-- brc example, p.229
brc_example :: Contract
brc_example = brc term nominal strike barrier underlying
  where
    term = 100.00 -- mkdate 2018 1 1
    nominal = 1000
    strike = 100
    barrier = 80
    ratio = 10
    underlying = times ratio one

main :: IO ()
main = L.putStr $ encode rc_example
