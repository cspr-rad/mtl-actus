import Lean.Data.RBMap

structure State where
  idx : Nat
  deriving BEq, Hashable, Repr
structure ClockVar where
  name : Nat
  deriving BEq, Hashable, Repr, Ord, DecidableEq
structure Clock where
  t : Nat
  deriving BEq, Hashable, Repr, Ord, DecidableEq
def Clock.le (x : Clock) (y : Nat) : Bool := x.t <= y
def Clock.lt (x : Clock) (y : Nat) : Bool := x.t < y
def Clock.ge (x : Clock) (y : Nat) : Bool := x.t >= y
def Clock.gt (x : Clock) (y : Nat) : Bool := x.t > y
def Clock.incr (x : Clock) (y : Nat) : Clock := { t := x.t + y }
instance : OfNat Clock 0 where
  ofNat := { t := 0 }

-- a limitation: alphabet is singleton rather than `Lean.HashSet`

def ClockMap := Lean.RBMap ClockVar Clock (fun _ _ => Ordering.lt)

inductive GuardOp : Type :=
| le
| lt
| ge
| gt

-- TODO: change to `structure ExecutionEntry` and `def Execution := List ExecutionEntry`
structure Execution (Alphabet : Type) where
  states : List State
  symbols : List Alphabet
  clocks : List ClockMap

def Trace Alphabet := List Alphabet
