module Main where

import Contracts (Contract (..), Currency (..), Asset (..), Stock (..), one)
import Control.Monad.IO.Class (liftIO)
import Derivatives (american, european, OptionKind (..))
import Models (P(..), runFutT)
import Valuation (evalC)

-- | main: some examples for now
main :: IO ()
main = do
  putStrLn "=================== European Call on X ==================="
  let euCallOnX = european Call 1 10.0 CHF (one (Stk X)) :: Contract
  print euCallOnX

  let P q = evalC (Cur CHF) euCallOnX :: P c IO Double
  r <- runFutT q
  print $ show r

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
