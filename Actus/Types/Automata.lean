import Lean.Data.RBMap
import Actus.Types.Classes

variable {Alphabet : Type} [AtomicProp Alphabet]

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

namespace Execution
  -- TODO: change to `structure ExecutionEntry` and `def Execution := List ExecutionEntry`
  structure Entry where
    state : State
    symbol : Alphabet
    clockMap : ClockMap

  def Fragment := List (@Entry Alphabet)

  def TraceFragment := List Alphabet
  def Fragment.toTrace (e : @Fragment Alphabet) : @TraceFragment Alphabet :=
    e.map (fun x => x.symbol)

  def PathFragment := List State
  def Fragment.toPath (e : @Fragment Alphabet) : PathFragment :=
    e.map (fun x => x.state)

  def ClocksFragment := List ClockMap
  def Fragment.toClocks (e : @Fragment Alphabet) : ClocksFragment :=
    e.map (fun x => x.clockMap)

end Execution
