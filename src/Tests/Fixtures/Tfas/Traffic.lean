import Actus

inductive TrafficLightState
  | RegularTransition
  | PedestrianButtonPress
  deriving BEq, Hashable, Repr

instance : AtomicProp TrafficLightState where

open TrafficLightState

def trafficTfa : TimedFinite.TFA TrafficLightState :=
  let red := State.mk 0
  let yellow := State.mk 1
  let green := State.mk 2

  let c0 := ClockVar.mk 0

  let states := Lean.HashSet.empty.insert red
                |>.insert yellow
                |>.insert green

  let alphabet := Lean.HashSet.empty.insert RegularTransition  -- Regular transition
                  |>.insert PedestrianButtonPress  -- Pedestrian button press

  let transitions := Lean.HashSet.empty.insertMany [
    -- Red to Green after 60 seconds
    { source := red
    , target := green
    , symbol := [RegularTransition]
    , guards := GuardConditions.mk {[ { clock := c0, op := GuardOp.ge, bound := 60 } ]}
    , reset := [c0]
    },
    -- Green to Yellow after 50 seconds
    { source := green
    , target := yellow
    , symbol := [RegularTransition]
    , guards := GuardConditions.mk {[ { clock := c0, op := GuardOp.ge, bound := 50 } ]}
    , reset := [c0]
    },
    -- Yellow to Red after 5 seconds
    { source := yellow
    , target := red
    , symbol := [RegularTransition]
    , guards := GuardConditions.mk {[ { clock := c0, op := GuardOp.ge, bound := 5 } ]}
    , reset := [c0]
    },
    -- Green to Yellow on pedestrian button press (if at least 30 seconds have passed)
    { source := green
    , target := yellow
    , symbol := [PedestrianButtonPress]
    , guards := GuardConditions.mk {[ { clock := c0, op := GuardOp.ge, bound := 30 } ]}
    , reset := [c0]
    }
  ]

  { states,
    alphabet,
    initialState := red,
    transitions,
    acceptingStates := states  -- All states are accepting in this case
  }

