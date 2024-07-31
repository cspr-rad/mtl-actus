-- import Actus
import Tests

def tests : List (TestM Unit) := [
  ClockMapLookup.test,
  GuardConditionEval.test,
  ValuationUpdate.test,
  IsValidTransition.test,
  ExecuteValidTransition.test,
  TrafficEmpty.test,
  TrafficNormal.test,
  TrafficPedestrian.test,
  TrafficInvalidQuick.test,
  TrafficEarlyPedestrian.test,
  TrafficLongCycle.test,
  Pam.test,
]

-- def clock_map_lookup := name_test ClockMapLookup.test

def main : IO Unit := do
  IO.println s!"Lean4 test runner about to run {tests.length} tests"
  for test in tests do
    printTestRun test

  IO.println "Debug output:"
  let b <- TrafficInvalidQuick.Debug.acceptsIo
  IO.println s!"TrafficInvalidQuick.Debug.acceptsIo = {b} (ought to be false)"
 
  IO.println "Debug output:"
  let b <- Pam.Debug.acceptsIo
  IO.println s!"Pam.Debug.acceptsIo = {b}"
