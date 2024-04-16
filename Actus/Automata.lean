import Lean.Data.RBMap
import Lean.Data.HashSet
import Actus.Types

namespace TimedBuchi
  variable (Alphabet : Type) [AtomicProp Alphabet]

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

  structure GuardCondition where
    clock : ClockVar
    op : GuardOp
    bound : Nat

  structure Transition where
    source : State
    target : State
    symbol : Alphabet
    guard : GuardCondition
    reset : List ClockVar

  structure TBA where
    states : List State
    alphabet : List Alphabet
    initialState : State
    transitions : List (Transition Alphabet)
    acceptingStates : List State

  def eval (gc : GuardCondition) (clockValues : ClockMap) : Bool :=
    match clockValues.find? gc.clock with
    | none => false
    | some clockValue =>
      match gc.op with
      | GuardOp.le => clockValue.le gc.bound
      | GuardOp.lt => clockValue.lt gc.bound
      | GuardOp.ge => clockValue.ge gc.bound
      | GuardOp.gt => clockValue.gt gc.bound

  def step (tba : TBA Alphabet) (state : State) (symbol : Alphabet) (clockValues : ClockMap) :
    List (State × ClockMap) :=
    tba.transitions.filterMap fun transition =>
      if transition.source == state && transition.symbol == symbol && eval transition.guard clockValues then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) clockValues
        some (transition.target, newClockValues)
      else
        none

  def accepts (tba : TBA Alphabet) (word : List Alphabet) : Bool := Id.run do
    let mut currentState := tba.initialState
    let mut clockValues : ClockMap := Lean.RBMap.empty
    for symbol in word do
      let nextStates := step _ tba currentState symbol clockValues
      match nextStates with
      | [] => return false
      | (newState, newClockValues) :: _ => do
        currentState := newState
        clockValues := newClockValues
        for (clockLabel, clock) in clockValues.toList do
          clockValues := clockValues.insert clockLabel (clock.incr 1)
    return tba.acceptingStates.contains currentState

end TimedBuchi
