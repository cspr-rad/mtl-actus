import Actus
import Tests.Runner

def c0 : ClockVar := ClockVar.mk 0
def c1 : ClockVar := ClockVar.mk 1
def c2 : ClockVar := ClockVar.mk 2
def clockValues1 : ClockMap := Lean.AssocList.empty.insert c0 (Clock.mk 5)

-- Transition fixtures
inductive sigma : Type
| alpha : sigma
| beta : sigma
deriving BEq, Hashable
instance : AtomicProp sigma where

def s0 : State := State.mk 0
def s1 : State := State.mk 1

def transition1 : @TimedFinite.Transition sigma := {
  source := s0,
  target := s1,
  symbol := sigma.alpha,
  guard := { clock := c1, op := GuardOp.lt, bound := 10 },
  reset := []
}
def timedLetter1 : @Execution.TimedLetter sigma := { symbol := sigma.alpha, time := { t := 2 } }
def prev_t1 : Option FiniteTimestamp := none

def transition2 : @TimedFinite.Transition sigma := {
  source := s1,
  target := State.mk 2,
  symbol := sigma.beta,
  guard := { clock := c0, op := GuardOp.ge, bound := 4 },
  reset := [c0]
}
def timedLetter2 : @Execution.TimedLetter sigma := { symbol := sigma.beta, time := { t := 10 } }
def prev_t2 : Option FiniteTimestamp := some (FiniteTimestamp.mk 6)

namespace ClockMapLookup
  def testCases : List (ClockVar × Nat) := [
      (c0, 5),
      (c1, 0)
  ]
  def test : TestM Unit := do
    for (clockVar, expected) in testCases do
      let result := (clockValues1.find? clockVar).getD 0
      assert (result.tick == expected) s!"ClockMapLookup: Expected {expected}, got {result.tick}"
end ClockMapLookup

namespace GuardConditionEval
  def testCases : List (GuardCondition × Bool) := [
    (GuardCondition.mk c0 GuardOp.le 6, true),
    (GuardCondition.mk c0 GuardOp.lt 6, true),
    (GuardCondition.mk c0 GuardOp.ge 5, true),
    (GuardCondition.mk c0 GuardOp.gt 4, true),
    (GuardCondition.mk c0 GuardOp.le 4, false),
    (GuardCondition.mk c1 GuardOp.le 6, true), -- uninitialized clock should be zero, and c <= 6
    (GuardCondition.mk c1 GuardOp.gt 1, false)
  ]
  def test : TestM Unit := do
    for (guard, expected) in testCases do
      let result := guard.eval clockValues1
      assert (result == expected) s!"GuardConditionEval: Expected {expected}, got {result}"
end GuardConditionEval

namespace ValuationUpdate
  def testCases : List (Nat × Nat) := [
    (TimedFinite.valuationUpdate (clockValues1.find? c0) (FiniteTimestamp.mk 2) (some (FiniteTimestamp.mk 1)), 6),
    (TimedFinite.valuationUpdate none (FiniteTimestamp.mk 2) none, 2),
    (TimedFinite.valuationUpdate (clockValues1.find? c1) (FiniteTimestamp.mk 2) (some (FiniteTimestamp.mk 1)), 1)
  ]
  def test : TestM Unit := do
    for (result, expected) in testCases do
      assert (result == expected) s!"ValuationUpdate: Expected {expected}, got {result}"
end ValuationUpdate

namespace IsValidTransition
  def testCases : List (Bool × Bool) := [
    (transition1.isValidTransition _ s0 timedLetter1 clockValues1, true),
    (transition2.isValidTransition _ s1 timedLetter2 clockValues1, true),
    (transition1.isValidTransition _ s0 timedLetter1 clockValues1, true),
    (transition2.isValidTransition _ s1 timedLetter2 clockValues1, true),
    (transition1.isValidTransition _ s0 timedLetter2 clockValues1, false),
    (transition2.isValidTransition _ s1 timedLetter1 clockValues1, false)
  ]
  def test : TestM Unit := do
    for (result, expected) in testCases do
      assert (result == expected) s!"IsValidTransition: Expected {expected}, got {result}"
end IsValidTransition

namespace ExecuteValidTransition
  def expectedClockValues1 : ClockMap := Lean.AssocList.empty.insert c0 (Clock.mk 5)
  def expectedClockValues2 : ClockMap := (Lean.AssocList.empty.insert c0 0).insert c1 { tick := 4 }
  -- guards are irrelevant
  def testCases : List ((ClockMap × State) × (ClockMap × State)) := [
    -- first in a word (prev_t is none), no clock reset
    (
      TimedFinite.executeValidTransition _ clockValues1 transition1,
      (expectedClockValues1, State.mk 1)
    ),
    -- with a clock reset
    (
      TimedFinite.executeValidTransition _ clockValues1 transition2,
      (expectedClockValues2, State.mk 2)
    )
  ]
  def test : TestM Unit := do
    for ((resultClockValues, resultState), (expectedClockValues, expectedState)) in testCases do
      for (resultClockVar, resultClock) in resultClockValues.toList do
        for (expectedClockVar, expectedClock) in expectedClockValues.toList do
          if resultClockVar == expectedClockVar then
            assert (resultClock == expectedClock)
              s!"ExecuteValidTransition Clock at {repr resultClockVar}: Expected {repr expectedClock}, got {repr resultClock}"
      assert (resultState == expectedState)
        s!"ExecuteValidTransition State: Expected {repr expectedState}, got {repr resultState}"
    assert (transition2.reset.contains c0) s!"beq on ClockVars is failing"
    assert ((clockValues1.toList.filter fun (cv, _) => transition2.reset.contains cv).length > 0)
      s!"{repr transition2.reset} is not filtering clockValues right"
end ExecuteValidTransition

