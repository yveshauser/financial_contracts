{-# LANGUAGE OverloadedStrings #-}

module Serialization where

import Contracts

import Data.Aeson

instance ToJSON Currency
instance ToJSON Stock

instance ToJSON Contract where
  toJSON Zero               = object []
  toJSON (One (Currency k)) = object [ "curr" .= k ]
  toJSON (One (Stock s))    = object [ "stock" .= s ]
  toJSON (Give c)           = object [ "give" .= toJSON c ]
  --toJSON (o `Scale` c)      = object [ "factor" .= o, "scale" .= toJSON c ]
  toJSON (c1 `And` c2)      = object [ "and" .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
{-
eval :: Contract -> [Stock]
eval Zero           = []
eval (One (Currency k)) = []
eval (One (Stock s)) = [s]
eval (Give c)       = eval c
eval (o `Scale` c)  = eval c
eval (c1 `And` c2)  = (eval c1) ++ (eval c2)
eval (c1 `Or` c2)   = (eval c1) ++ (eval c2)
eval (Cond o c1 c2) = (eval c1) ++ (eval c2)
eval (When o c)     = eval c
eval (Anytime o c)  = eval c
eval (Until o c)    = eval c
-}
