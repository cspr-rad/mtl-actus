import Actus
import Tests.Runner
import Tests.Helpers
import Tests.Fixtures.Words.Empty
import Tests.Fixtures.Tfas.Traffic

def R : TrafficLightState := TrafficLightState.RegularTransition
def P : TrafficLightState := TrafficLightState.PedestrianButtonPress

namespace TrafficEmpty
  def word : @Execution.TimedWord TrafficLightState := emptyWord TrafficLightState
  def accepts : Bool := trafficTfa.accepts _ word
  def test : TestM Unit := assert accepts "The empty word failed on the traffic TFA, even though all states are accepting states"
end TrafficEmpty

namespace TrafficNormal
  def word : @Execution.TimedWord TrafficLightState := mkTimedWord [
    mkTimedLetter R 60, -- Red to Green
    mkTimedLetter R 110, -- Green to Yellow
    mkTimedLetter R 115 -- Yellow to Red
  ]
  def accepts : Bool := trafficTfa.accepts _ word
  def test : TestM Unit := assert accepts "A run with no pedestrian button presses has failed to be accepted"
end TrafficNormal

namespace TrafficPedestrian
  def word : @Execution.TimedWord TrafficLightState := mkTimedWord [
    mkTimedLetter R 60,  -- Red to Green
    mkTimedLetter P 95,  -- Pedestrian button press (after 35s of Green)
    mkTimedLetter R 100  -- Yellow to Red
  ]
  def accepts : Bool := trafficTfa.accepts _ word
  def test : TestM Unit := assert accepts "A run with a valid pedestrian button press has failed to be accepted"
end TrafficPedestrian

namespace TrafficInvalidQuick
  def word : @Execution.TimedWord TrafficLightState := mkTimedWord [
    mkTimedLetter R 30,  -- Trying to change Red to Green too early
    mkTimedLetter R 40,  -- Trying to change Green to Yellow too early
    mkTimedLetter R 45   -- Trying to change Yellow to Red too early
  ]
  def accepts : Bool := trafficTfa.accepts _ word
  -- #guard !accepts
  def test : TestM Unit := assert (!accepts) "An invalid run with transitions occurring too quickly has been incorrectly accepted"
end TrafficInvalidQuick

namespace TrafficEarlyPedestrian
  def word : @Execution.TimedWord TrafficLightState := mkTimedWord [
    mkTimedLetter R 60,  -- Red to Green
    mkTimedLetter P 80,  -- Pedestrian button press (after 20s of Green, should be ignored)
    mkTimedLetter R 110, -- Green to Yellow (normal timing)
    mkTimedLetter R 115  -- Yellow to Red
  ]
  def accepts : Bool := trafficTfa.accepts _ word
  def test : TestM Unit := assert accepts "A run with an early pedestrian button press (which should be ignored) has failed to be accepted"
end TrafficEarlyPedestrian

namespace TrafficLongCycle
  def word : @Execution.TimedWord TrafficLightState := mkTimedWord [
    mkTimedLetter R 60,   -- Red to Green
    mkTimedLetter R 110,  -- Green to Yellow
    mkTimedLetter R 115,  -- Yellow to Red
    mkTimedLetter R 175,  -- Red to Green
    mkTimedLetter R 225,  -- Green to Yellow
    mkTimedLetter R 230   -- Yellow to Red
  ]
  def accepts : Bool := trafficTfa.accepts _ word
  def test : TestM Unit := assert accepts "A run with two full cycles has failed to be accepted"
end TrafficLongCycle
