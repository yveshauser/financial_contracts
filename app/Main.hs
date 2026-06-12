module Main where

import Contracts (Contract (..), Currency (..), Asset (..), Stock (..), one)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Massiv.Array as M
import Derivatives (american, european, OptionKind (..))
import Models (ModelChoice (..), P(..), greeksWith, runFutT)
import System.Environment (getArgs)
import System.Exit (die)
import Valuation (evalC)

parseModel :: String -> Maybe ModelChoice
parseModel "bs"   = Just BlackScholes
parseModel "crr"  = Just CRR
parseModel "jr"   = Just JR
parseModel "jrrn" = Just JRrn
parseModel "tian" = Just Tian
parseModel "mc"   = Just MonteCarlo
parseModel "lsmc" = Just LSMC
parseModel _      = Nothing

-- | main: some examples for now
main :: IO ()
main = do
  args <- getArgs
  model <- case args of
    [name] | Just m <- parseModel name -> pure m
    _ -> die "usage: models <bs|crr|jr|jrrn|tian|mc|lsmc>"
  putStrLn $ "Using model: " ++ show model

  putStrLn "=================== European Call on X ==================="
  let euCallOnX = european Call 1 10.0 CHF (one (Stk X)) :: Contract
  print euCallOnX

  let P q = evalC (Cur CHF) euCallOnX :: P c IO Double
  r <- runFutT (q model)
  putStrLn $ "Value: " ++ show (M.evaluate' r 0)

  greeks <- runFutT (greeksWith model)
  unless (null greeks) $
    putStrLn $ "Greeks (AD): "
            ++ intercalate ", " [name ++ " = " ++ show v | (name, v) <- greeks]

  putStrLn "=================== European Call on Z ==================="
  let euCallOnZ = european Call 120 100.0 CHF (one (Stk Z)) :: Contract
  print euCallOnZ

  putStrLn "=================== European Put on X ==================="
  let euPutOnX = european Put 1 14.0 CHF (one (Stk X)) :: Contract
  print euPutOnX

  putStrLn "=================== American Call on X ==================="
  let amCallOnX = american Call (0, 0) 1 CHF (one (Stk X))
  print amCallOnX

  return ()
