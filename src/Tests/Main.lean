import Actus
import Tests

def tests : List (TestM Unit) := [
  ClockMapLookup.test,
  GuardConditionEval.test,
  ValuationUpdate.test,
  ExecuteValidTransition.test,
  TrafficEmpty.test,
  TrafficNormal.test,
  TrafficPedestrian.test,
  TrafficInvalidQuick.test,
  TrafficEarlyPedestrian.test,
  TrafficLongCycle.test,
  testPam,
]

def main : IO Unit := do
  IO.println s!"Lean4 test runner about to run {tests.length} tests"
  for test in tests do
    printTestRun test

  IO.println "Debug output:"
  let b <- TrafficNormal.Debug.acceptsIo
  IO.println s!"TrafficNormal.Debug.acceptsIo = {b}"
