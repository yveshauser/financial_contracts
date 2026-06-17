{-# LANGUAGE GADTs, OverloadedStrings #-}

-- | Projected cash-flows from a 'Contract'.
--
-- This is a purely /structural/ projection: a fold over the contract syntax
-- (a sibling of 'Valuation.evalC', but where 'evalC' collapses the contract
-- into a value process, this enumerates the contractual payments). It needs no
-- market data and no pricing model. Amounts and conditions that depend on
-- observables are kept symbolically as 'Obs' expressions.
--
-- The projection enumerates /all/ branches: an 'Or' or 'Anytime' is a choice
-- for the holder, so every alternative flow is emitted, tagged (via 'cfChoices')
-- with the selection that leads to it. A 'Cond' emits both arms, each guarded
-- by the (negated) condition. The caller decides which branch is realised.
--
-- Caveats, all driven by keeping the projection model-free:
--
--   * 'When' gates: a temporal gate (at/after/before/between) fixes the
--     settlement time, with the innermost winning when nested (we do not
--     intersect windows symbolically); a non-temporal gate, e.g. a barrier
--     @value u %< b@, is recorded as a guard so it survives an inner temporal
--     gate (as in a down-and-in barrier option).
--   * 'Until' (knock-out) is recorded as a guard @not o@ on the inner flows
--     rather than a true absorbing barrier.
--   * 'Anytime' settlement is reported as the exercise /window/; the actual
--     exercise date is the holder's (model-dependent) choice.
module Cashflows
  ( CashFlow (..)
  , TimeSpec (..)
  , cashflows
  , renderCashFlows
  ) where

import Prelude hiding (until)
import Contracts
import Data.List (intercalate)

-- | When a projected flow settles, as far as it can be read off the gating
-- observable without a model.
data TimeSpec
  = Now                 -- ^ acquired immediately (no enclosing temporal gate)
  | OnDate Time         -- ^ exactly at @t@           (@when (at t)@)
  | InWindow Time Time  -- ^ any time in @[t1,t2]@     (@anytime (between t1 t2)@)
  | FromDate Time       -- ^ at or after @t@           (@when (after t)@)
  | UntilDate Time      -- ^ before @t@                (@when (before t)@)
  | WhenObs (Obs Bool)  -- ^ gated by a non-temporal observable we could not reduce
  deriving Show

-- | A single projected payment.
data CashFlow = CashFlow
  { cfTime    :: TimeSpec    -- ^ when the payment settles
  , cfAsset   :: Asset       -- ^ the asset paid or received
  , cfAmount  :: Obs Double  -- ^ symbolic amount (product of enclosing scales); @konst 1@ if unscaled
  , cfSign    :: Double      -- ^ @+1@ received (long) / @-1@ paid (after an odd number of 'Give's)
  , cfGuards  :: [Obs Bool]  -- ^ conditions that must all hold for the flow to occur
  , cfChoices :: [String]    -- ^ optional-branch selections on the path to this flow
  }

-- | State threaded down the contract tree while projecting.
data Ctx = Ctx
  { ctxSign    :: Double
  , ctxScale   :: Obs Double
  , ctxTime    :: TimeSpec
  , ctxGuards  :: [Obs Bool]
  , ctxChoices :: [String]
  }

-- | Project the cash-flows of a contract, in structural (pre-order) order.
cashflows :: Contract -> [CashFlow]
cashflows = go ctx0
  where
    ctx0 = Ctx { ctxSign = 1, ctxScale = konst 1, ctxTime = Now, ctxGuards = [], ctxChoices = [] }

    go :: Ctx -> Contract -> [CashFlow]
    go ctx c = case c of
      Zero        -> []
      One a       -> [ CashFlow (ctxTime ctx) a (ctxScale ctx) (ctxSign ctx)
                                (ctxGuards ctx) (ctxChoices ctx) ]
      Give x      -> go ctx { ctxSign = negate (ctxSign ctx) } x
      And a b     -> go ctx a ++ go ctx b
      Or a b      -> go (choose "or:left"  ctx) a ++ go (choose "or:right" ctx) b
      Cond o a b  -> go (guardWith o ctx) a ++ go (guardWith (notObs o) ctx) b
      Scale o x   -> go ctx { ctxScale = mulObs (ctxScale ctx) o } x
      When o x    -> go (whenGate o ctx) x
      Anytime o x -> go (choose ("exercise " ++ showTime (timeOf o)) ctx) { ctxTime = timeOf o } x
      Until o x   -> go (guardWith (notObs o) ctx) x

    choose tag ctx    = ctx { ctxChoices = ctxChoices ctx ++ [tag] }
    guardWith o ctx   = ctx { ctxGuards  = ctxGuards ctx ++ [o] }

    -- A temporal gate (at/after/before/between) fixes the settlement time
    -- (innermost wins); a non-temporal gate -- e.g. a barrier @value u %< b@ --
    -- is a condition that must hold, recorded as a guard so it survives an
    -- inner temporal 'When' rather than being silently overwritten.
    whenGate o ctx = case timeOf o of
      WhenObs g -> guardWith g ctx
      spec      -> ctx { ctxTime = spec }

-- | Read a settlement 'TimeSpec' off a gating observable. Recognises the
-- temporal primitives and the @after t1 && before t2@ window ('between'),
-- and keeps anything else as a symbolic gate.
timeOf :: Obs Bool -> TimeSpec
timeOf o = case o of
  At t     -> OnDate t
  After t  -> FromDate t
  Before t -> UntilDate t
  Lift2 "(&&)" _ (After t1)  (Before t2) -> InWindow t1 t2
  Lift2 "(&&)" _ (Before t2) (After t1)  -> InWindow t1 t2
  _        -> WhenObs o

-- | Logical negation lifted into 'Obs', tagged for display.
notObs :: Obs Bool -> Obs Bool
notObs = lift "not" not

-- | Multiply two amount observables, dropping a @konst 1@ identity so that
-- unscaled flows read as @1.0@ rather than @1.0 * 1.0@.
mulObs :: Obs Double -> Obs Double -> Obs Double
mulObs a b
  | isOne a   = b
  | isOne b   = a
  | otherwise = lift2 "(*)" (*) a b
  where
    isOne (Konst x) = x == 1
    isOne _         = False

-- Rendering -----------------------------------------------------------------

-- | A human-readable, one-line-per-flow rendering of a projection, e.g.
--
-- > on 1: receive 1.0 × (Stk X)            [or:left]
-- > on 1: pay     10.0 × (Cur EUR)          [or:left]
renderCashFlows :: [CashFlow] -> String
renderCashFlows [] = "(no cash-flows)"
renderCashFlows cs = intercalate "\n" (map renderCashFlow cs)

renderCashFlow :: CashFlow -> String
renderCashFlow cf = unwords $
     [ showTime (cfTime cf) ++ ":"
     , dir (cfSign cf)
     , inlineObs (cfAmount cf)
     , "×"
     , "(" ++ show (cfAsset cf) ++ ")"
     ]
  ++ [ "if " ++ intercalate " ∧ " (map inlineObs (cfGuards cf)) | not (null (cfGuards cf)) ]
  ++ [ "[" ++ intercalate ", " (cfChoices cf) ++ "]"            | not (null (cfChoices cf)) ]
  where
    dir s | s < 0     = "pay    "
          | otherwise = "receive"

showTime :: TimeSpec -> String
showTime t = case t of
  Now          -> "now"
  OnDate d     -> "on " ++ show d
  InWindow a b -> "in [" ++ show a ++ ".." ++ show b ++ "]"
  FromDate d   -> "from " ++ show d
  UntilDate d  -> "before " ++ show d
  WhenObs o    -> "when " ++ inlineObs o

-- | Render an observable on a single line, showing lifted binary operators
-- infix (e.g. @value(one (Stk X)) < 90.0@) and other lifts as prefix
-- application (e.g. @not at 1@). Composite operands are parenthesised.
inlineObs :: Obs a -> String
inlineObs o = case o of
  Konst a       -> show a
  At t          -> "at "     ++ show t
  After t       -> "after "  ++ show t
  Before t      -> "before " ++ show t
  Value c       -> "value(" ++ inlineContract c ++ ")"
  Lift s _ a    -> s ++ " " ++ operand a
  Lift2 s _ a b -> case stripOp s of
    Just op -> operand a ++ " " ++ op ++ " " ++ operand b
    Nothing -> s ++ " " ++ operand a ++ " " ++ operand b

-- | Render a sub-observable as an operand, parenthesising the composite
-- (lifted) cases so the surrounding operator's structure stays unambiguous.
operand :: Obs a -> String
operand o = case o of
  Lift{}  -> "(" ++ inlineObs o ++ ")"
  Lift2{} -> "(" ++ inlineObs o ++ ")"
  _       -> inlineObs o

-- | An operator symbol is tagged in parens (e.g. @"(<)"@, @"(&&)"@); strip
-- them to recover the infix form. Prefix lifts (@"not"@, @"abs"@) are untagged.
stripOp :: String -> Maybe String
stripOp ('(' : rest) | not (null rest), last rest == ')' = Just (init rest)
stripOp _                                                = Nothing

-- | Collapse a contract's tree rendering onto one line (for @value(...)@).
inlineContract :: Contract -> String
inlineContract c = case renderContract c of
  [single] -> single
  ls       -> unwords (map (dropWhile isBox) ls)
  where
    isBox ch = ch `elem` ("│├└─ " :: String)
