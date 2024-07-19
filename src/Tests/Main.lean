import Actus
import Tests

def main : IO Unit := do
  IO.println "Lean4 Test Runner"
  printTestRun testPam
  printTestRun TrafficEmpty.test
  printTestRun TrafficNormal.test
  printTestRun TrafficPedestrian.test
  printTestRun TrafficInvalidQuick.test
  printTestRun TrafficEarlyPedestrian.test
  printTestRun TrafficLongCycle.test
