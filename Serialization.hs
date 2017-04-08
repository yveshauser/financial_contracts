{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialization where

--import Contracts
--import Stocks
--import Currencies
--import Assets

--import Data.Aeson

-- instance ToJSON Currency
-- instance ToJSON Stock

{-#
instance ToJSON (Obs o) where
  toJSON (Konst a)       = toJSON $ show a
  toJSON (Lift _ o)      = object [ "lift" .= toJSON o ]
  toJSON (Lift2 _ o1 o2) = object [ "lift2" .= object [ "o1" .= toJSON o1, "o2" .= toJSON o2 ]]
  toJSON (Value c)       = object [ "value" .= toJSON c ]
  toJSON (At t)          = object [ "at" .= toJSON t ]
  toJSON (Before t)      = object [ "before" .= toJSON t ]
  toJSON (After t)       = object [ "when" .= toJSON t ]

instance ToJSON Contract where
  toJSON Zero               = object []
  toJSON (One (Currency k)) = object [ "curr" .= k ]
  toJSON (One (Stock s))    = object [ "stock" .= s ]
  toJSON (Give c)           = object [ "give" .= toJSON c ]
  toJSON (o `Scale` c)      = object [ "factor" .= o, "scale" .= toJSON c ]
  toJSON (c1 `And` c2)      = object [ "and" .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
  toJSON (c1 `Or` c2)       = object [ "or" .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
  toJSON (Cond o c1 c2)     = object [ "cond" .= o, "do" .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
  toJSON (When o c)         = object [ "when" .= o, "do" .= toJSON c ]
  toJSON (Anytime o c)      = object [ "anytime" .= o, "do" .= toJSON c ]
  toJSON (Until o c)        = object [ "until" .= o, "do" .= toJSON c ]
#-}
