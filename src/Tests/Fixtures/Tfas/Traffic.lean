import Actus

inductive TrafficLightState
  | RegularTransition
  | PedestrianButtonPress
  deriving BEq, Hashable

instance : AtomicProp TrafficLightState where

open TrafficLightState

def trafficTfa : TimedFinite.TFA TrafficLightState :=
  let red := State.mk 0
  let yellow := State.mk 1
  let green := State.mk 2

  let clock := ClockVar.mk 0

  let states := Lean.HashSet.empty.insert red
                |>.insert yellow
                |>.insert green

  let alphabet := Lean.HashSet.empty.insert RegularTransition  -- Regular transition
                  |>.insert PedestrianButtonPress  -- Pedestrian button press

  let transitions := [
    -- Red to Green after 60 seconds
    { source := red
    , target := green
    , symbol := RegularTransition
    , guard := { clock := clock, op := GuardOp.ge, bound := 60 }
    , reset := [clock]
    },
    -- Green to Yellow after 50 seconds
    { source := green
    , target := yellow
    , symbol := RegularTransition
    , guard := { clock := clock, op := GuardOp.ge, bound := 50 }
    , reset := [clock]
    },
    -- Yellow to Red after 5 seconds
    { source := yellow
    , target := red
    , symbol := RegularTransition
    , guard := { clock := clock, op := GuardOp.ge, bound := 5 }
    , reset := [clock]
    },
    -- Green to Yellow on pedestrian button press (if at least 30 seconds have passed)
    { source := green
    , target := yellow
    , symbol := PedestrianButtonPress
    , guard := { clock := clock, op := GuardOp.ge, bound := 30 }
    , reset := [clock]
    }
  ]

  { states := states
  , alphabet := alphabet
  , initialState := red
  , transitions := transitions
  , acceptingStates := states  -- All states are accepting in this case
  }

