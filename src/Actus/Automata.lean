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
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.evalD entry.clockMap then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues, cashFlow := none}
      else
        none

  def TFA.stepWithCashFlow (tfa : TFA Alphabet) (entry : @Execution.Entry Alphabet) (cashFlow : Alphabet -> Money) : @Execution.Fragment Alphabet :=
    tfa.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.evalD entry.clockMap then
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

  def TFA.accepts' (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) (initLocation : State) (initClockMap : ClockMap) : Bool := Id.run do
     false

  def TFA.accepts (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) : Bool := Id.run do
    let mut currentState := tfa.initialState
    let mut clockValues : ClockMap := Lean.RBMap.ofList (tfa.transitions.map
        (fun transition => (transition.guard.clock, 0)))
    for timedLetter in word.letters do
      for transition in tfa.transitions do
        let mut validTransition := false
        if transition.source == currentState ∧ transition.symbol == timedLetter.symbol then
          let incrBy := (clockValues.findD transition.guard.clock 0).tick + timedLetter.time.t -- need minus previous timestamp somehow
          let guardSatisfied := transition.guard.eval clockValues incrBy
          if guardSatisfied then
            for (clockVar, clock) in clockValues.toList do
              -- this is weird cuz tick is a timestamp that's being interpreted as delta
              clockValues := clockValues.insert clockVar (clock.incr timedLetter.time.t)
              if transition.reset.contains clockVar then
                clockValues := clockValues.insert clockVar 0
            validTransition := true
            currentState := transition.target
            for clockLabel in transition.reset do
              clockValues := clockValues.insert clockLabel 0
            break
        if !validTransition then
          return false
    return tfa.acceptingStates.contains currentState
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
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.evalD entry.clockMap then
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
