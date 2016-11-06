{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, DeriveGeneric #-}

module Termsheet where

import Data.Map hiding (map)
import Data.Maybe
import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Aeson
import GHC.Generics

import Contracts
import Contracts.BRC
import Currencies
import Stocks
import Assets

stocks = fromList([(A,"bla bla A"), (B, "bla bla B"), (C, "bla bla C")])

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

is_zcb :: Contract -> Bool
is_zcb (When _ (Scale (Konst _) (One _))) = True
is_zcb _ = False

is_short_put_barrier_option :: Contract -> Bool
is_short_put_barrier_option c = True

is_brc :: Contract -> Bool
is_brc (c1 `And` c2) | is_zcb c1 && is_short_put_barrier_option c2 = True
is_brc (c2 `And` c1) | is_zcb c1 && is_short_put_barrier_option c2 = True
is_brc _ = False

data Language = DE | FR | IT | EN

termsheet :: Metadata -> Contract -> Language -> Termsheet
termsheet m c l | is_brc c = toTS l b
  where b = BRC {
        valor_ = valor m
      , underlyings_ = ppUnderlyings c
      , barrier_ = 0.8 }

-- ppContract :: Contract -> IO ()
-- ppContract c | is_brc c = let b = BRC in ppTermsheet b

ppTermsheet :: Termsheetable t => Language -> t -> IO ()
ppTermsheet l = printTS . (toTS l)

ppUnderlyings :: Contract -> [String]
ppUnderlyings c = catMaybes $ map lookup' $ eval c
  where lookup' = (flip lookup) stocks

data Metadata = Metadata {
    valor :: String
  , isin :: String
  , initialFixing :: String
  , finalFixing :: String
  , issuer :: String
                         } deriving (Show, Generic)


-- type Termsheet = (String, String)
type Termsheet = [(String, [(String, String)])]

printTS :: Termsheet -> IO ()
printTS ts = mapM_ f ts
  where f (s, t) = putStrLn s >> mapM_ g t
        g (s, t) = putStrLn ("  " ++ s ++ ": " ++ t)

class Termsheetable a where
  toTS :: Language -> a -> Termsheet

instance Termsheetable BRC where
  toTS DE brc = [("Beschreibung", [("Valor", valor_ brc), ("Basiswerte", show (underlyings_ brc))]), ("Zusammenfassung", [("test", "test")])]
  toTS FR brc = undefined
  toTS IT brc = undefined
  toTS EN brc = undefined


-- TODO: use mustache template?




















instance FromJSON Metadata
instance ToJSON Metadata

ppMetadata :: Metadata -> IO ()
ppMetadata = C.putStr . encode

data StructuredProduct = StructuredProduct {
    metadata :: Metadata
  , underlyings :: [String]
                                           } deriving (Generic)
instance FromJSON StructuredProduct
instance ToJSON StructuredProduct

defaultMetadata = Metadata "12345" "CH123456" "1.1.2017" "1.1.2017" "issuer"

ppStructuredProduct :: StructuredProduct -> IO ()
ppStructuredProduct s = C.putStr $ encode s

test_ts = ppStructuredProduct $ StructuredProduct m (ppUnderlyings c)
  where m = defaultMetadata
        c = (One (Stock A)) `And` (One (Stock B))
