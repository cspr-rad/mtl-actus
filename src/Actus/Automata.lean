import Lean.Data.AssocList
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

  -- v_{i-1} + t_i - t_{i-1} if i-1 exists and t_i otherwise
  def valuationUpdate (v : Option Clock) (ti : FiniteTimestamp) (tIMinus1 : Option FiniteTimestamp) : Nat :=
   (v.getD 0).tick + ti.t - (tIMinus1.getD { t := 0 }).t

  def Transition.isValidTransition
      (t : Transition Alphabet)
      (currentState : State)
      (timedLetter : @Execution.TimedLetter Alphabet)
      (clockValues : ClockMap) : Bool :=
    t.source == currentState && t.symbol == timedLetter.symbol && t.guard.eval clockValues

  /--
    Assumes currentState == t.source and t.symbol == timedLetter.symbol
  --/
  def executeValidTransition
      (clockValues : ClockMap)
      (transition : Transition Alphabet)
      (timedLetter : @Execution.TimedLetter Alphabet)
      (prev_t : Option FiniteTimestamp)
      : ClockMap × State := Id.run do
    let mut clockValuesMut := clockValues.mapVal
        fun c => let incrBy :=
          valuationUpdate (clockValues.find? transition.guard.clock) timedLetter.time prev_t;
          c.incr incrBy
    for (clockVar, _) in clockValuesMut.toList.filter fun (cv, _) => transition.reset.contains cv do
      clockValuesMut := clockValuesMut.replace clockVar 0

    return (clockValuesMut, transition.target)

  def TFA.accepts'
      (tfa : TFA Alphabet)
      (word : @Execution.TimedWord Alphabet)
      (entryState : State)
      (initClockMap : ClockMap)
      : Bool := Id.run do
    let mut currentState := entryState
    let mut clockValues := initClockMap
    let mut prev_t : Option FiniteTimestamp := none
    let mut validTransition := word.letters.length == 0
    for timedLetter in word.letters do
      for transition in tfa.transitions.filter
          fun t => t.isValidTransition _ currentState timedLetter clockValues do
        let execution := executeValidTransition _ clockValues transition timedLetter prev_t
        clockValues := execution.1
        currentState := execution.2
        validTransition := true
      prev_t := some timedLetter.time
    return tfa.acceptingStates.contains currentState && validTransition

  def TFA.accepts (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) : Bool :=
    let clockValues : ClockMap :=
      (tfa.transitions.map fun transition => (transition.guard.clock, 0)).foldl
        (fun acc (k, v) => acc.insert k v) Lean.AssocList.empty
    tfa.accepts' _ word tfa.initialState clockValues

  def TFA.acceptsDebug
      (tfa : TFA Alphabet)
      (word : @Execution.TimedWord Alphabet)
      : IO Bool := do
    let clockValues : ClockMap :=
      (tfa.transitions.map fun transition => (transition.guard.clock, 0)).foldl
        (fun acc (k, v) => acc.insert k v) Lean.AssocList.empty
    let mut currentState := tfa.initialState
    let mut clockValues := clockValues
    let mut prev_t : Option FiniteTimestamp := none
    let mut validTransition := word.letters.length == 0

    for timedLetter in word.letters do
      IO.println s!"Processing letter..."
      for transition in tfa.transitions do
        IO.println s!"Processing transition: t.source==currentState: {transition.source == currentState} | t.symbol==timedLetter.symbol: {transition.symbol == timedLetter.symbol} | t.guard.eval clockValues: {transition.guard.eval clockValues}"
      for transition in tfa.transitions.filter
          (fun t => t.isValidTransition _ currentState timedLetter clockValues) do
        IO.println s!"Valid transition found"

        let execution := executeValidTransition _ clockValues transition timedLetter prev_t
        clockValues := execution.1
        currentState := execution.2
        validTransition := true

        IO.println s!"New state: {repr currentState}"

      prev_t := some timedLetter.time
      IO.println s!"Updated prev_t: {repr prev_t}"

    return tfa.acceptingStates.contains currentState && validTransition

  structure ValidRun where
    tfa : TFA Alphabet
    word : @Execution.TimedWord Alphabet
    run : @Execution.Run Alphabet
    initialization (H : 0 < run.entries.length) : tfa.initialState = (run.entries[0]'H).l0
    consecution1 : forall (i : Nat), exists (t : Transition Alphabet), true -- TODO
    consecution2 : forall (i : Nat), exists (t : Transition Alphabet), true -- TODO

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
    let mut clockValues : ClockMap := Lean.AssocList.empty
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
