module Main where

import Prelude hiding (until)
import Contracts (Contract (..), Currency (..), Asset (..), Stock (..), one, until, at)
import Control.Monad (unless)
import Data.List (intercalate)
import Derivatives (american, european, brc, OptionKind (..))
import Models (ModelChoice (..), priceWith, defaultMarket, greeksWith, runFutT)
import System.Environment (getArgs)
import System.Exit (die)

parseModel :: String -> Maybe ModelChoice
parseModel "bs"   = Just BlackScholes
parseModel "crr"  = Just CRR
parseModel "jr"   = Just JR
parseModel "jrrn" = Just JRrn
parseModel "tian" = Just Tian
parseModel "mc"   = Just MonteCarlo
parseModel "lsmc" = Just LSMC
parseModel "lattice" = Just Lattice
parseModel _      = Nothing

-- | main: some examples for now
main :: IO ()
main = do
  args <- getArgs
  model <- case args of
    [name] | Just m <- parseModel name -> pure m
    _ -> die "usage: models <bs|crr|jr|jrrn|tian|mc|lsmc|lattice>"
  putStrLn $ "Using model: " ++ show model

  -- priceWith values the contract with the chosen model: a vanilla European
  -- option goes to that model's scalar pricer; anything else falls back to the
  -- lattice value process (evalC threaded through Futhark).
  let value con = runFutT (priceWith (Cur CHF) model defaultMarket con)

  putStrLn "=================== European Call on X ==================="
  let euCallOnX = european Call 1 10.0 CHF (one (Stk X)) :: Contract
  print euCallOnX
  value euCallOnX >>= \r -> putStrLn $ "Value: " ++ show r   -- when -> disc

  greeks <- runFutT (greeksWith model defaultMarket euCallOnX)
  unless (null greeks) $
    putStrLn $ "Greeks (AD): "
            ++ intercalate ", " [name ++ " = " ++ show v | (name, v) <- greeks]

  putStrLn "=================== European Put on X ==================="
  let euPutOnX = european Put 1 14.0 CHF (one (Stk X)) :: Contract
  print euPutOnX
  value euPutOnX >>= \r -> putStrLn $ "Value: " ++ show r

  putStrLn "=================== American Put on X ==================="
  -- anytime -> snell (the lattice Snell envelope, early exercise)
  let amPutOnX = american Put (0, 1) 100.0 CHF (one (Stk X))
  print amPutOnX
  value amPutOnX >>= \r -> putStrLn $ "Value: " ++ show r

  putStrLn "=================== One share of X (exch) ==================="
  -- one -> exch (the spot value of the asset)
  value (one (Stk X)) >>= \r -> putStrLn $ "Value: " ++ show r

  putStrLn "=================== Knock-out share of X (absorb) ==================="
  -- until -> absorb (the knock-out operator)
  let koShareOfX = until (at 1) (one (Stk X))
  print koShareOfX
  value koShareOfX >>= \r -> putStrLn $ "Value: " ++ show r

  putStrLn "=================== Barrier Reverse Convertible on X ==================="
  -- a structured product (Derivatives.brc): a zero-coupon bond, short a
  -- down-and-in put. Combines and/give/when/scale/value over the value process.
  -- brc maturity nominal strike barrier currency underlying
  let brcOnX = brc 1 1000 100 80 CHF (one (Stk X))
  print brcOnX
  value brcOnX >>= \r -> putStrLn $ "Value: " ++ show r

  return ()
