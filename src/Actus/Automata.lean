import Lean.Data.RBMap
import Lean.Data.HashSet
import Actus.Types.Automata
import Actus.Types.Numbers
import Actus.Types.Classes

namespace TimedFinite
  variable (Alphabet : Type) [AtomicProp Alphabet]

  structure Transition where
    source : State
    target : State
    symbol : Alphabet
    guard : GuardCondition
    reset : List ClockVar
    deriving BEq, Hashable

  structure TFA where
    states : Lean.HashSet State
    alphabet : Lean.HashSet Alphabet
    initialState : State
    transitions : List (Transition Alphabet)
    acceptingStates : Lean.HashSet State

  def TFA.step (tfa : TFA Alphabet) (entry : @Execution.Entry Alphabet) : @Execution.Fragment Alphabet :=
    tfa.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.eval entry.clockMap then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues, cashFlow := none}
      else
        none

  def TFA.stepWithCashFlow (tfa : TFA Alphabet) (entry : @Execution.Entry Alphabet) (cashFlow : Alphabet -> Money) : @Execution.Fragment Alphabet :=
    tfa.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.eval entry.clockMap then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues, cashFlow := cashFlow entry.symbol }
      else
        none

  def TFA.isValidFragment (tfa : TFA Alphabet) (exec : @Execution.Fragment Alphabet) : Bool :=
    match exec with
    | [] => true
    | entry :: rest =>
      let nextEntries := tfa.step _ entry;
      let anyNextEntries := nextEntries.any
        fun nextEntry => match rest.head? with
            | none => false
            | some entry => entry.symbol == nextEntry.symbol && entry.state == nextEntry.state
      anyNextEntries && isValidFragment tfa rest

  -- v_{i-1} + t_i - t_{i-1} if t_{i-1} exists and t_i otherwise
  -- The v argument will be zero if uninitialized on the caller's side, and nonzero if otherwise initialized.
  def valuationUpdate (v : Nat) (ti : Nat) (tIMinus1 : Option Nat) : Nat :=
    v + ti - tIMinus1.getD 0
   
  def TFA.accepts' (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) (entryState : State) (initClockMap : ClockMap) : Bool := Id.run do
    let mut currentState := entryState
    let mut clockValues := initClockMap
    let mut prev_t : Option FiniteTimestamp := none
    let mut validTransition := false
    for timedLetter in word.letters do
      let relevantTransitions := tfa.transitions.filter (fun t => t.source == currentState ∧ t.symbol == timedLetter.symbol)
      for transition in relevantTransitions do
        if transition.guard.eval clockValues then
          validTransition := true
          let incrBy := valuationUpdate
              (clockValues.findD transition.guard.clock 0).tick
              timedLetter.time.t
              (prev_t.map (fun t => t.t))
          for (clockVar, clock) in clockValues.toList do
            clockValues := clockValues.insert clockVar (clock.incr incrBy)
            if transition.reset.contains clockVar then
              clockValues := clockValues.insert clockVar 0
          currentState := transition.target
          break -- This means that we're doing effectively deterministic TFA
      prev_t := some timedLetter.time
    return tfa.acceptingStates.contains currentState

  def TFA.accepts (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) : Bool :=
    let clockValues : ClockMap := Lean.RBMap.ofList (tfa.transitions.map
        (fun transition => (transition.guard.clock, 0)));
    tfa.accepts' _ word tfa.initialState clockValues

  structure ValidRun where
    tfa : TFA Alphabet
    word : @Execution.TimedWord Alphabet
    run : @Execution.Run Alphabet
    initialization (H : 0 < run.entries.length) : tfa.initialState = (run.entries[0]'H).l0
    consecution1 : forall (i : Nat), true -- TODO
    consecution2 : forall (i : Nat), true -- TODO

end TimedFinite

-- timed buchi only comes into play when we think about composing contracts, each of which are finite,
-- into an infinite time-horizon stream of contracts representing the global financial system
namespace TimedBuchi
  variable (Alphabet : Type) [AtomicProp Alphabet]

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

  def TBA.step (tba : TBA Alphabet) (entry : @Execution.Entry Alphabet) : List (@Execution.Entry Alphabet) :=
    tba.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.eval entry.clockMap then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues, cashFlow := none}
      else
        none

  def TBA.isValidFragment (tba : TBA Alphabet) (exec : @Execution.Fragment Alphabet) : Bool :=
    match exec with
    | [] => true
    | entry :: rest =>
      let nextEntries := step _ tba entry;
      let anyNextEntries := nextEntries.any
        fun nextEntry => match rest.head? with
            | none => false
            | some entry => entry.symbol == nextEntry.symbol && entry.state == nextEntry.state
      anyNextEntries && isValidFragment tba rest

  -- def acceptsUntimedNaive (tba : TBA Alphabet) (exec : @Execution.Fragment Alphabet) : Bool :=
  --   isValidFragment _ tba exec && (exec.getLast? |>.map (fun entry => tba.acceptingStates.contains entry.state) |>.getD false)

  def TBA.acceptsUntimed (tba : TBA Alphabet) (word : List Alphabet) : Bool := Id.run do
    let mut currentState := tba.initialState
    let mut clockValues : ClockMap := Lean.RBMap.empty
    for symbol in word do
      let nextStates := step _ tba { state := currentState, symbol := symbol, clockMap := clockValues, cashFlow := none }
      match nextStates with
      | [] => return false
      | entry :: _ => do
        currentState := entry.state
        clockValues := entry.clockMap
        for (clockLabel, clock) in clockValues.toList do
          clockValues := clockValues.insert clockLabel (clock.incr 1)
    return tba.acceptingStates.contains currentState
end TimedBuchi
