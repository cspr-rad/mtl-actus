import Lean.Data.RBMap
import Actus.Types.Classes
import Actus.Types.Numbers
import Actus.Types.Time

variable {Alphabet : Type} [AtomicProp Alphabet]

structure State where
  idx : Nat
  deriving BEq, Hashable, Repr
structure ClockVar where
  name : Nat
  deriving BEq, Hashable, Repr, Ord, DecidableEq
structure Clock where
  tick : Nat
  deriving BEq, Hashable, Repr, Ord, DecidableEq
instance ClockLE : LE Clock where
  le x y := x.tick ≤ y.tick
instance : LT Clock where
  lt x y := x.tick < y.tick
theorem Clock.lt_trans {x y z : Clock} : x < y → y < z → x < z := Nat.lt_trans
def Clock.le (x : Clock) (y : Nat) : Bool := x.tick <= y
def Clock.lt (x : Clock) (y : Nat) : Bool := x.tick < y
def Clock.ge (x : Clock) (y : Nat) : Bool := x.tick >= y
def Clock.gt (x : Clock) (y : Nat) : Bool := x.tick > y
def Clock.incr (x : Clock) (y : Nat) : Clock := { tick := x.tick + y }
instance : OfNat Clock 0 where
  ofNat := { tick := 0 }

-- a limitation: alphabet is singleton rather than `Lean.HashSet`

def ClockMap := Lean.RBMap ClockVar Clock (fun _ _ => Ordering.lt)

inductive GuardOp : Type :=
  | le
  | lt
  | ge
  | gt
  deriving BEq, Hashable, Repr

structure GuardCondition where
  clock : ClockVar
  op : GuardOp
  bound : Nat
  deriving BEq, Hashable, Repr

def GuardCondition.eval (gc : GuardCondition) (clockValues : ClockMap) (incrBy : Nat) : Bool :=
  match clockValues.find? gc.clock with
  | none => false
  | some clockValue => let cv := clockValue.incr incrBy;
    match gc.op with
    | GuardOp.le => cv.le gc.bound
    | GuardOp.lt => cv.lt gc.bound
    | GuardOp.ge => cv.ge gc.bound
    | GuardOp.gt => cv.gt gc.bound

def GuardCondition.evalD (gc : GuardCondition) (clockValues : ClockMap) : Bool :=
  gc.eval clockValues 0

namespace Execution
  structure Entry where
    state : State
    symbol : Alphabet
    clockMap : ClockMap
    cashFlow : Option Money

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

  structure TimedLetter where
    symbol : Alphabet
    time : FiniteTimestamp
    deriving BEq, Hashable, Repr
  instance TimedLetterLE : LE (@TimedLetter Alphabet) where
    le x y := x.time ≤ y.time
  instance : LT (@TimedLetter Alphabet) where
    lt x y := x.time < y.time

  def isNonDecreasing (letters: List (@TimedLetter Alphabet)) := ∀ (i j : Nat) (H0 : j < letters.length) (H1 : i < j),
      let H2 : i < letters.length := Clock.lt_trans H1 H0;
      letters[i]'H2 ≤ letters[j]'H0

  structure TimedWord where
    letters : List (@TimedLetter Alphabet)
    nonDecreasing : isNonDecreasing letters
end Execution
