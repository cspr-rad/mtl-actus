-- Defunct and abandoned approach.
import Actus.Logic
import Actus.Automata

open MetricTemporal TimedFinite
variable {T : Type} [AtomicProp T]

def Proposition.nnf' (p : Proposition T) : Proposition T :=
  match p with
  | mtt => mtt
  | [[x]] => [[x]]
  | ~ φ' =>
    match φ' with
    | mtt => mtf
    | [[x]] => ~ [[x]]
    | ~ φ'' => nnf' φ''
    | φ and ψ => (nnf' (~ φ)) or (nnf' (~ ψ))
    | φ U ψ in w => (nnf' (~ φ)) S (nnf' (~ ψ)) in w
    | φ S ψ in w => (nnf' (~ φ)) U (nnf' (~ ψ)) in w
  | φ and ψ => (nnf' φ) and (nnf' ψ)
  | φ U ψ in w => (nnf' φ) U (nnf' ψ) in w
  | φ S ψ in w => (nnf' φ) S (nnf' ψ) in w

inductive TableauNode (T : Type) where
| leaf : Proposition T -> TableauNode T
| and : TableauNode T -> TableauNode T -> TableauNode T
| or : TableauNode T -> TableauNode T -> TableauNode T
| until : TableauNode T -> Window -> TableauNode T -> TableauNode T
| since : TableauNode T -> Window -> TableauNode T -> TableauNode T

def Proposition.toTableau {T : Type} (φ : Proposition T) : TableauNode T :=
  let rec toTableauRec (ψ : Proposition T) : TableauNode T :=
    match ψ with
    | mtt => .leaf mtt
    | [[x]] => .leaf [[x]]
    | ~ [[x]] => .leaf (~ [[x]])
    | φ' and ψ' => .and (toTableauRec φ') (toTableauRec ψ')
    | φ' U ψ' in w => .until (toTableauRec φ') w (toTableauRec ψ')
    | φ' S ψ' in w => .since (toTableauRec φ') w (toTableauRec ψ')
    | ~ (φ' and ψ') => .or (toTableauRec (~ φ')) (toTableauRec (~ ψ'))
    | _ => .leaf ψ -- Should be unreachable due to nnf correctness
  toTableauRec (nnf' φ)

def TableauNode.collectAtomicProps : TableauNode T -> Lean.HashSet T
| .leaf p => p.collectAtomicProps
| .and n1 n2 => n1.collectAtomicProps.merge n2.collectAtomicProps
| .or n1 n2 => n1.collectAtomicProps.merge n2.collectAtomicProps
| .until n1 _ n2 => n1.collectAtomicProps.merge n2.collectAtomicProps
| .since n1 _ n2 => n1.collectAtomicProps.merge n2.collectAtomicProps

namespace Beta
  def TableauNode.toTfa (node : TableauNode T) : TFA T :=
  let rec leafHelper (p : Proposition T) (stateCounter : Nat) (clockCounter : Nat) : (TFA T × State × Nat × Nat) :=
    let state := { idx := stateCounter }
    let states := Lean.HashSet.empty.insert state
    let transitions := match p with
      | [[x]] => Lean.HashSet.empty.insert { source := state, target := state, symbol := [x], guards := GuardConditions.mk {[ { clock := ⟨clockCounter⟩, op := .le, bound := 0 } ]}, reset := [] }
      | _ => Lean.HashSet.empty
    let alphabet := match p with
      | [[x]] => Lean.HashSet.empty.insert x
      | _ => Lean.HashSet.empty
    let tfa := {
        states,
        alphabet,
        initialState := state,
        acceptingStates := states,
        transitions,
    }
    (tfa, state, stateCounter + 1, clockCounter)
  let rec build (node' : TableauNode T) (stateCounter : Nat) (clockCounter : Nat) : (TFA T × State × Nat × Nat) :=
      match node' with
      | .leaf p => leafHelper p stateCounter clockCounter
      | .and n1 n2 =>
        let (tfa1, s1, sc1, cc1) := build n1 stateCounter clockCounter
        let (tfa2, s2, sc2, cc2) := build n2 sc1 cc1
        let newState := { idx := sc2 }
        let transitions := tfa1.transitions.toList
            ++ tfa2.transitions.toList
            ++ node'.collectAtomicProps.toList.map
              (fun x => {
                source := newState,
                target := s1,
                symbol := [x],
                guards := GuardConditions.mk {[ { clock := ⟨cc2⟩, op := .le, bound := 0 } ]},
                reset := []
              })
            ++ node'.collectAtomicProps.toList.map
              (fun x => {
                source := newState,
                target := s2,
                symbol := [x],
                guards := GuardConditions.mk {[ { clock := ⟨cc2⟩, op := .le, bound := 0 } ]},
                reset := []
              })
        let tfa := {
            states := tfa1.states.insert newState,
            alphabet := tfa1.alphabet.merge tfa2.alphabet,
            initialState := newState,
            transitions := Lean.HashSet.empty.insertMany transitions,
            acceptingStates := tfa1.acceptingStates.intersect tfa2.acceptingStates
        }
        (tfa, newState, sc2 + 1, cc2 + 1)
      | .or n1 n2 =>
        let (tfa1, s1, sc1, cc1) := build n1 stateCounter clockCounter
        let (tfa2, s2, sc2, cc2) := build n2 sc1 cc1
        let newState := { idx := sc2 }
        let transitions := tfa1.transitions.toList
              ++ tfa2.transitions.toList
              ++ node'.collectAtomicProps.toList.map
                (fun x => {
                  source := newState,
                  target := s1,
                  symbol := [x],
                  guards := GuardConditions.mk {[ { clock := ⟨cc2⟩, op := .le, bound := 0 } ]},
                  reset := []
                })
              ++ node'.collectAtomicProps.toList.map
                (fun x => {
                  source := newState,
                  target := s2,
                  symbol := [x],
                  guards := GuardConditions.mk {[ { clock := ⟨cc2⟩, op := .le, bound := 0 } ]},
                  reset := []
                })
        let tfa := {
            states := tfa1.states.insert newState,
            alphabet := tfa1.alphabet.merge tfa2.alphabet,
            initialState := newState,
            transitions := Lean.HashSet.empty.insertMany transitions,
            acceptingStates := tfa1.acceptingStates.merge tfa2.acceptingStates
        }
        (tfa, newState, sc2 + 1, cc2 + 1)
      | .until n1 w n2 =>
        let (tfa1, s1, sc1, cc1) := build n1 stateCounter clockCounter
        let (tfa2, s2, sc2, cc2) := build n2 sc1 cc1
        let lower := match w.2 with
          | .t x => x.toNat
          | _ => 0 -- TODO: handle infinity
        let upper := match w.1 with
          | .t x => x.toNat
          | _ => 999999999999999999 -- TODO: handle infinity
        let newState := { idx := sc2 }
        let untilClock := ⟨cc2⟩
        let transitions := tfa1.transitions.toList
            ++ tfa2.transitions.toList
            -- Transition to satisfy φ2 (right operand)
            ++ node'.collectAtomicProps.toList.map
              (fun x => {
                source := newState,
                target := s2,
                symbol := [x],
                guards := GuardConditions.mk (
                  {[ { clock := untilClock, op := .ge, bound := lower } ]}.append
                  {[ { clock := untilClock, op := .le, bound := upper } ]}
                ),
                reset := []
              })
            -- Continue to satisfy φ1 (left operand)
            ++ node'.collectAtomicProps.toList.map
              (fun x => {
                source := newState,
                target := s1,
                symbol := [x],
                guards := GuardConditions.mk {[ { clock := untilClock, op := .le, bound := upper } ]},
                reset := []
              })
            -- Self-loop to allow time to pass
            ++ node'.collectAtomicProps.toList.map
              (fun x => {
                source := newState,
                target := newState,
                symbol := [x],
                guards := GuardConditions.mk {[ { clock := untilClock, op := .le, bound := upper } ]},
                reset := [untilClock]
              })
         let tfa := {
            states := tfa1.states.insert newState,
            alphabet := tfa1.alphabet.merge tfa2.alphabet,
            initialState := newState,
            transitions := Lean.HashSet.empty.insertMany transitions,
            acceptingStates := tfa1.acceptingStates
        }
        (tfa, newState, sc2 + 1, cc2 + 1)
      | .since n1 w n2 => sorry
  (build node 0 0).1
end Beta
