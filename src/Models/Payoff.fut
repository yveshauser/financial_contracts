-- | Vanilla option payoffs
--
-- kind: 1 = call, 0 = put

def payoff (kind: i8) (k: f64) (s: f64): f64 =
  if kind == 1 then f64.max (s - k) 0 else f64.max (k - s) 0
