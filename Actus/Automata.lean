import Lean.Data.RBMap
import Lean.Data.HashSet
import Actus.Types.Automata
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
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.eval entry.clockMap { tick := 0 } then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues }
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

  -- def TFA.acceptsUntimedNaive (tfa : TFA Alphabet) (exec : @Execution.Fragment Alphabet) : Bool :=
  --   tfa.isValidFragment _ exec && (exec.getLast? |>.map (fun entry => tfa.acceptingStates.contains entry.state) |>.getD false)

  def TFA.accepts (tfa : TFA Alphabet) (word : @Execution.TimedWord Alphabet) : Bool := Id.run do
    let mut currentState := tfa.initialState
    let mut clockValues : ClockMap := Lean.RBMap.empty
    for timedLetter in word.letters do
      let mut validTransition := false
      for transition in tfa.transitions do
        if transition.source == currentState ∧ transition.symbol == timedLetter.symbol then
          let guardSatisfied := transition.guard.eval clockValues timedLetter.clock
          if guardSatisfied then
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
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.eval entry.clockMap { tick := 0 } then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues }
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
      let nextStates := step _ tba { state := currentState, symbol := symbol, clockMap := clockValues }
      match nextStates with
      | [] => return false
      | entry :: _ => do
        currentState := entry.state
        clockValues := entry.clockMap
        for (clockLabel, clock) in clockValues.toList do
          clockValues := clockValues.insert clockLabel (clock.incr 1)
    return tba.acceptingStates.contains currentState
end TimedBuchi
