import Lean.Data.RBMap
import Lean.Data.HashSet
import Actus.Types.Automata
import Actus.Types.Classes

-- important: we don't actually want a timed buchi because infinite horizon isn't so important.
namespace TimedBuchi
  variable (Alphabet : Type) [AtomicProp Alphabet]

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

  def GuardCondition.eval (gc : GuardCondition) (clockValues : ClockMap) : Bool :=
    match clockValues.find? gc.clock with
    | none => false
    | some clockValue =>
      match gc.op with
      | GuardOp.le => clockValue.le gc.bound
      | GuardOp.lt => clockValue.lt gc.bound
      | GuardOp.ge => clockValue.ge gc.bound
      | GuardOp.gt => clockValue.gt gc.bound

  def step (tba : TBA Alphabet) (entry : @Execution.Entry Alphabet) :
    List (@Execution.Entry Alphabet) :=
    tba.transitions.filterMap fun transition =>
      if transition.source == entry.state && transition.symbol == entry.symbol && transition.guard.eval entry.clockMap then
        let newClockValues := transition.reset.foldl (fun acc cv => acc.insert cv 0) entry.clockMap
        some { state := transition.target, symbol := entry.symbol, clockMap := newClockValues }
      else
        none

  def isValidFragment (tba : TBA Alphabet) (exec : @Execution.Fragment Alphabet) : Bool :=
    match exec with
    | [] => true
    | entry :: rest =>
      let nextEntries := step _ tba entry
      nextEntries.any fun nextEntry => let rh := rest.head?;
        rh.map (fun entry => entry.symbol == nextEntry.symbol && entry.state == nextEntry.state) |>.getD false &&
       isValidFragment tba rest

  def accepts (tba : TBA Alphabet) (exec : @Execution.Fragment Alphabet) : Bool :=
    isValidFragment _ tba exec && (exec.getLast? |>.map (fun entry => tba.acceptingStates.contains entry.state) |>.getD false)

-- def accepts (tba : TBA Alphabet) (word : List Alphabet) : Bool := Id.run do
--   let mut currentState := tba.initialState
--   let mut clockValues : ClockMap := Lean.RBMap.empty
--   for symbol in word do
--     let nextStates := step _ tba currentState symbol clockValues
--     match nextStates with
--     | [] => return false
--     | (newState, newClockValues) :: _ => do
--       currentState := newState
--       clockValues := newClockValues
--       for (clockLabel, clock) in clockValues.toList do
--         clockValues := clockValues.insert clockLabel (clock.incr 1)
--   return tba.acceptingStates.contains currentState
--
-- def isValidExecution (tba : TBA Alphabet) (exec : Execution Alphabet) : Bool :=
--   if exec.states.length != exec.symbols.length + 1 || exec.clocks.length != exec.symbols.length then
--     false
--   else
--     let stateTransitions := exec.states.zip (exec.states.drop 1)
--     let symbolClockPairs := exec.symbols.zip exec.clocks
--     stateTransitions.zip symbolClockPairs |>.all fun ((source, target), (symbol, clockValues)) =>
--       step _ tba source symbol clockValues |>.any fun (nextState, _) =>
--         nextState == target
--
-- def executionToTrace (exec : Execution Alphabet) : Trace Alphabet :=
--   exec.symbols
--
-- def acceptsExecution (tba : TBA Alphabet) (exec : Execution Alphabet) : Bool :=
--   isValidExecution _ tba exec && (exec.states.getLast?.map (fun lastState => tba.acceptingStates.contains lastState) |>.getD false)
--
-- def acceptsTrace (tba : TBA Alphabet) (trace : Trace Alphabet) : Bool :=
--   accepts _ tba trace

end TimedBuchi
