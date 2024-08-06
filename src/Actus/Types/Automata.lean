import Actus.Data.AssocList
import Actus.Data.NonEmptyList
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
 instance ClockVarLE : LE ClockVar where
  le x y := x.name ≤ y.name
structure Clock where
  tick : Nat
  deriving BEq, Hashable, Repr, Ord, DecidableEq
instance ClockInhabited : Inhabited Clock where
  default := { tick := 0 }
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
def ClockMap : Type := Lean.AssocList ClockVar Clock

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

structure GuardConditions where
  conditions : NonEmptyList GuardCondition
  deriving BEq, Hashable, Repr
def GuardConditions.map (f : GuardCondition → GuardCondition) (gcs : GuardConditions) : GuardConditions :=
  { conditions := gcs.conditions.map f }

def GuardCondition.eval (gc : GuardCondition) (clockValues : ClockMap) : Bool :=
  let clockValue: Clock := match clockValues.find? gc.clock with
  | none => { tick := 0 }
  | some clockValue => clockValue
  match gc.op with
  | GuardOp.le => clockValue.le gc.bound
  | GuardOp.lt => clockValue.lt gc.bound
  | GuardOp.ge => clockValue.ge gc.bound
  | GuardOp.gt => clockValue.gt gc.bound

def GuardConditions.eval (gcs : GuardConditions) (clockValues : ClockMap) : Bool :=
  gcs.conditions.all (fun gc => gc.eval clockValues)

inductive Alternation {α : Type} : Type where
| tru : Alternation
| fls : Alternation
| and : Alternation → Alternation → Alternation
| or : Alternation → Alternation → Alternation
| var : α → Alternation
| cond : GuardCondition -> Alternation
| reset_in : ClockVar -> Alternation -> Alternation
  deriving BEq, Hashable, Repr

-- to lift this from one-clock, we probably replace the `v` indexing the `|=` with a `ClockMap`. This would be a breaking change.
inductive Alternation.Eval {α : Type} (M : α × Clock) : ClockMap -> @Alternation α -> Prop where
| tru : forall v, Alternation.Eval M v Alternation.tru
| and : forall v a b, Alternation.Eval M v a -> Alternation.Eval M v b -> Alternation.Eval M v (Alternation.and a b)
| or_left : forall v a b, Alternation.Eval M v a -> Alternation.Eval M v (Alternation.or a b)
| or_right : forall v a b, Alternation.Eval M v b -> Alternation.Eval M v (Alternation.or a b)
| var : forall (v : ClockMap), v.toList.contains ({name:=0}, M.2) -> Alternation.Eval M v (Alternation.var M.1)
| cond : forall v (gc : GuardCondition), (exists (cv : ClockMap), gc.eval cv = true) -> Alternation.Eval M v (Alternation.cond gc)
| reset_in : forall v, Alternation.Eval M (Lean.AssocList.empty.insert {name:=0} {tick:=0}) a -> Alternation.Eval M v (Alternation.reset_in { name := 0 } a)

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

  structure RunEntry where
    l0 : State
    v0 : FiniteTimestamp
    sigma : @TimedLetter Alphabet
    l1 : State
    v1 : FiniteTimestamp

  structure Run where
    entries : List (@RunEntry Alphabet)
    chained : ∀ (i : Nat) (H : i + 1 < entries.length), entries[i].l1 = entries[i+1].l0
end Execution
