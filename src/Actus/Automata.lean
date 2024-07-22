import Actus.Data.HashSet
import Actus.Data.AssocList
import Actus.Types.Automata
import Actus.Types.Numbers
import Actus.Types.Classes

namespace TimedFinite
  variable (Alphabet : Type) [AtomicProp Alphabet]

  structure Transition where
    source : State
    target : State
    symbol : Alphabet
    guards : GuardConditions
    reset : List ClockVar
    deriving BEq, Hashable, Repr

  structure TFA where
    states : Lean.HashSet State
    alphabet : Lean.HashSet Alphabet
    initialState : State
    transitions : List (Transition Alphabet)
    acceptingStates : Lean.HashSet State

  def TFA.step (tfa : TFA Alphabet) (entry : @Execution.Entry Alphabet) : @Execution.Fragment Alphabet :=
    tfa.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guards.eval entry.clockMap then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues, cashFlow := none}
      else
        none

  def TFA.stepWithCashFlow (tfa : TFA Alphabet) (entry : @Execution.Entry Alphabet) (cashFlow : Alphabet -> Money) : @Execution.Fragment Alphabet :=
    tfa.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guards.eval entry.clockMap then
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
    t.source == currentState && t.symbol == timedLetter.symbol && t.guards.eval clockValues

  def incrStep
      (clockValues : ClockMap)
      (guard : GuardCondition)
      (timedLetter : @Execution.TimedLetter Alphabet)
      (prev_t : Option FiniteTimestamp)
      : ClockMap :=
    clockValues.mapVal
        fun c => let incrBy :=
          valuationUpdate (clockValues.find? guard.clock) timedLetter.time prev_t;
        c.incr incrBy

  /--
    Assumes currentState == t.source and t.symbol == timedLetter.symbol
  --/
  def executeValidTransition
      (clockValues : ClockMap)
      (transition : Transition Alphabet)
      : ClockMap × State := Id.run do
    let mut clockValuesMut := clockValues
    for (clockVar, _) in clockValuesMut.toList.filter
        fun (cv, _) => transition.reset.contains cv do
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
    let mut clockValuesUpdate := clockValues
    let mut prev_t : Option FiniteTimestamp := none
    let mut validTransition := word.letters.length == 0
    -- problem: we need to reject words that are nonempty but make no valid transitions.
    for timedLetter in word.letters do
      for transition in tfa.transitions do
        for guard in transition.guards.conditions.toList do
          -- TODO: why does `clockValues` here pass tests but not `clockValuesUpdate`?
          clockValuesUpdate := incrStep _ clockValues guard timedLetter prev_t
        if transition.isValidTransition _ currentState timedLetter clockValuesUpdate then
          (clockValues, currentState) := executeValidTransition _ clockValuesUpdate transition
          validTransition := true
          break
      prev_t := some timedLetter.time
    return tfa.acceptingStates.contains currentState && validTransition

  def TFA.accepts (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) : Bool :=
    let clockValues : ClockMap := tfa.transitions.foldl
        (fun acc t => t.guards.conditions.foldl
          (fun acc g => acc.insertOrReplace g.clock 0)
        acc)
        Lean.AssocList.empty
    tfa.accepts' _ word tfa.initialState clockValues

  def TFA.acceptsDebug
      (tfa : TFA Alphabet)
      (word : @Execution.TimedWord Alphabet)
      : IO Bool := do
    let clockValues : ClockMap := tfa.transitions.foldl
        (fun acc t => t.guards.conditions.foldl
          (fun acc g => acc.insertOrReplace g.clock 0)
        acc)
        Lean.AssocList.empty
    let mut currentState := tfa.initialState
    let mut clockValuesMut := clockValues
    let mut clockValuesUpdate := clockValues
    let mut prev_t : Option FiniteTimestamp := none
    let mut validTransition := word.letters.length == 0
    IO.println s!"num transitions: {tfa.transitions.length}"
    for timedLetter in word.letters do
      IO.println s!"Processing letter {repr timedLetter}..."
      for transition in tfa.transitions do
        IO.println s!"▸Checking transition {repr transition}..."
        for guard in transition.guards.conditions.toList do
          clockValuesUpdate := incrStep _ clockValuesMut guard timedLetter prev_t
        if transition.isValidTransition _ currentState timedLetter clockValuesUpdate then
          IO.println s!"▸▸Valid transition found"
          (clockValuesMut, currentState) := executeValidTransition _ clockValuesUpdate transition
          validTransition := true
          IO.println s!"▸▸New state: {repr currentState}"
          break

      prev_t := some timedLetter.time
      IO.println s!"▸Updated prev_t: {repr prev_t}"

    return tfa.acceptingStates.contains currentState && validTransition

--  structure ValidRun where
--    tfa : TFA Alphabet
--    word : @Execution.TimedWord Alphabet
--    run : @Execution.Run Alphabet
--    initialization (H : 0 < run.entries.length) : tfa.initialState = (run.entries[0]'H).l0
--    consecution1 : forall (i : Nat), exists (t : Transition Alphabet), true -- TODO
--    consecution2 : forall (i : Nat), exists (t : Transition Alphabet), true -- TODO

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
        let newClockValues := transition.reset.foldl (fun acc cv => acc.replace cv 0) entry.clockMap
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
          clockValues := clockValues.replace clockLabel (clock.incr 1)
    return tba.acceptingStates.contains currentState
end TimedBuchi
