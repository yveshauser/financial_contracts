module Main where

import Prelude hiding (until)
import Contracts (Contract (..), Currency (..), Asset (..), Stock (..), one, until, at)
import Cashflows (cashflows, renderCashFlows)
import Control.Monad (unless)
import Data.List (intercalate)
import Derivatives (american, european, down_and_in, OptionKind (..))
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

-- | The example contracts, each with a heading, shared by the pricing and
-- cash-flow projection modes.
examples :: [(String, Contract)]
examples =
  [ ("European Call on X",  european Call 1 10.0 CHF (one (Stk X)))
  , ("European Put on X",   european Put  1 14.0 CHF (one (Stk X)))
  , ("American Put on X",   american Put (0, 1) 100.0 CHF (one (Stk X)))
  , ("Down-and-in Put on X (barrier 90)",
       down_and_in 90.0 (one (Stk X)) (european Put 1 100.0 CHF (one (Stk X))))
  , ("One share of X",      one (Stk X))
  , ("Knock-out share of X", until (at 1) (one (Stk X)))
  ]

heading :: String -> IO ()
heading t = putStrLn $ "=================== " ++ t ++ " ==================="

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--cashflows"] -> runCashflows
    [name] | Just m <- parseModel name -> runPricing m
    _ -> die "usage: models <bs|crr|jr|jrrn|tian|mc|lsmc|lattice> | --cashflows"

-- | Project and print the contractual cash-flows of each example. Purely
-- structural: no model and no market data are involved.
runCashflows :: IO ()
runCashflows =
  mapM_ projection examples
  where
    projection (title, con) = do
      heading title
      print con
      putStrLn $ renderCashFlows (cashflows con)
      putStrLn ""

-- | Price (and, for vanillas, show greeks of) each example with the chosen model.
runPricing :: ModelChoice -> IO ()
runPricing model = do
  putStrLn $ "Using model: " ++ show model

  -- priceWith values the contract with the chosen model: a vanilla European
  -- option goes to that model's scalar pricer; anything else falls back to the
  -- lattice value process (evalC threaded through Futhark).
  let value con = runFutT (priceWith (Cur CHF) model defaultMarket con)

  mapM_ (priceOne model value) examples

-- | Price one example and, where the model supplies them (vanilla europeans),
-- print its greeks. 'greeksWith' returns @[]@ for contracts it does not cover.
priceOne :: ModelChoice -> (Contract -> IO Double) -> (String, Contract) -> IO ()
priceOne model value (title, con) = do
  heading title
  print con
  value con >>= \r -> putStrLn $ "Value: " ++ show r

  greeks <- runFutT (greeksWith model defaultMarket con)
  unless (null greeks) $
    putStrLn $ "Greeks (AD): "
            ++ intercalate ", " [name ++ " = " ++ show v | (name, v) <- greeks]
