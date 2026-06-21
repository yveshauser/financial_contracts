-- | Golden tests pinning the structural cash-flow projection ('Cashflows')
-- for the example contracts (the same set as @models --cashflows@), plus a
-- 'Cond' case exercising both guarded arms and the negated branch condition.
--
-- Purely structural: no model and no market data, so this suite is fast and
-- has no Futhark dependency.
module Main where

import Prelude hiding (until)
import Contracts (Currency (..), Asset (..), Stock (..), Contract, one, until, at, cond, times, give)
import Cashflows (cashflows, renderCashFlows)
import Control.Monad (unless)
import Data.List (intercalate)
import Derivatives (american, european, down_and_in, eur, chf, OptionKind (..))
import System.Exit (exitFailure)

-- | (label, contract, expected rendering of its projection).
cases :: [(String, Contract, [String])]
cases =
  [ ( "European Call on X"
    , european Call 1 10.0 CHF (one (Stk X))
    , [ "on 1: receive 1.0 × (Stk X) [or:left]"
      , "on 1: pay     10.0 × (Cur CHF) [or:left]" ] )

  , ( "European Put on X"
    , european Put 1 14.0 CHF (one (Stk X))
    , [ "on 1: pay     1.0 × (Stk X) [or:left]"
      , "on 1: receive 14.0 × (Cur CHF) [or:left]" ] )

  , ( "American Put on X"
    , american Put (0, 1) 100.0 CHF (one (Stk X))
    , [ "in [0..1]: pay     1.0 × (Stk X) [exercise in [0..1]]"
      , "in [0..1]: receive 100.0 × (Cur CHF) [exercise in [0..1]]" ] )

  , ( "Down-and-in Put on X (barrier survives inner temporal gate as a guard)"
    , down_and_in 90.0 (one (Stk X)) (european Put 1 100.0 CHF (one (Stk X)))
    , [ "on 1: pay     1.0 × (Stk X) if value(one (Stk X)) < 90.0 [or:left]"
      , "on 1: receive 100.0 × (Cur CHF) if value(one (Stk X)) < 90.0 [or:left]" ] )

  , ( "One share of X"
    , one (Stk X)
    , [ "now: receive 1.0 × (Stk X)" ] )

  , ( "Knock-out share of X"
    , until (at 1) (one (Stk X))
    , [ "now: receive 1.0 × (Stk X) if not at 1" ] )

  , ( "Conditional (both arms, negated guard)"
    , cond (at 2) (times 50 eur) (give (times 20 chf))
    , [ "now: receive 50.0 × (Cur EUR) if at 2"
      , "now: pay     20.0 × (Cur CHF) if not at 2" ] )
  ]

main :: IO ()
main = do
  oks <- mapM check cases
  unless (and oks) exitFailure

check :: (String, Contract, [String]) -> IO Bool
check (name, con, expected) = do
  let want = intercalate "\n" expected
      got  = renderCashFlows (cashflows con)
      ok   = got == want
  putStrLn $ (if ok then "PASS  " else "FAIL  ") ++ name
  unless ok $ do
    putStrLn "  expected:"; putStrLn (indent want)
    putStrLn "  got:";      putStrLn (indent got)
  pure ok

indent :: String -> String
indent = intercalate "\n" . map ("    " ++) . lines
