import Actus
import Tests

def tests : List (TestM Unit) := [
  testPam,
  TrafficEmpty.test,
  TrafficNormal.test,
  TrafficPedestrian.test,
  TrafficInvalidQuick.test,
  TrafficEarlyPedestrian.test,
  TrafficLongCycle.test,
]

def main : IO Unit := do
  IO.println s!"Lean4 test runner about to run {tests.length} tests"
  for test in tests do
    printTestRun test
