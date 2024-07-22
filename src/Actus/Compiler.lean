import Actus.Logic
import Actus.Automata

open MetricTemporal TimedFinite
variable {T : Type} [AtomicProp T]

def Proposition.nnf (p : Proposition T) : Proposition T :=
  match p with
  | mtt => mtt
  | [[x]] => [[x]]
  | ~ φ' =>
    match φ' with
    | mtt => mtf
    | [[x]] => ~ [[x]]
    | ~ φ'' => nnf φ''
    | φ and ψ => (nnf (~ φ)) or (nnf (~ ψ))
    | φ U ψ in w => (nnf (~ φ)) S (nnf (~ ψ)) in w
    | φ S ψ in w => (nnf (~ φ)) U (nnf (~ ψ)) in w
  | φ and ψ => (nnf φ) and (nnf ψ)
  | φ U ψ in w => (nnf φ) U (nnf ψ) in w
  | φ S ψ in w => (nnf φ) S (nnf ψ) in w

namespace NnfCorrect
  theorem doubleNegationCorrect (φ : Proposition T) : MetricTemporalSemantics.equiv φ (~ (~ φ)) := by
    unfold MetricTemporalSemantics.equiv <;> intros
    apply Iff.intro <;> intro H
    . sorry
    . sorry
  -- lemma for demorgan.
  theorem nnfCorrect (φ : Proposition T) : MetricTemporalSemantics.equiv φ (Proposition.nnf φ) := by
    unfold MetricTemporalSemantics.equiv <;> intros
    sorry
end NnfCorrect

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
  toTableauRec (nnf φ)

def TableauNode.collectAtomicProps : TableauNode T -> Lean.HashSet T
| .leaf p => p.collectAtomicProps
| .and n1 n2 => n1.collectAtomicProps.merge n2.collectAtomicProps
| .or n1 n2 => n1.collectAtomicProps.merge n2.collectAtomicProps
| .until n1 _ n2 => n1.collectAtomicProps.merge n2.collectAtomicProps
| .since n1 _ n2 => n1.collectAtomicProps.merge n2.collectAtomicProps

namespace Beta
--   def TableauNode.toTfa (node : TableauNode T) : TFA T :=
--   let rec build (node' : TableauNode T) (stateCounter : Nat) (clockCounter : Nat) : (TFA T × State × Nat × Nat) :=
--       match node' with
--       | .leaf p =>
--       let state := { idx := stateCounter }
--       let states := Lean.HashSet.empty.insert state
--       let tfa := {
--           states,
--           alphabet := Lean.HashSet.empty.insert p
--           initialState := state,
--           acceptingStates := states,
--           transitions := [],
--       }
--       (tfa, state, stateCounter + 1, clockCounter)
--       | .and n1 n2 =>
--       let (tfa1, s1, sc1, cc1) := build n1 stateCounter clockCounter
--       let (tfa2, s2, sc2, cc2) := build n2 sc1 cc1
--       let newState := { idx := sc2 }
--       let tfa := {
--           states := tfa1.states.insert newState,
--           alphabet := tfa1.alphabet.merge tfa2.alphabet,
--           initialState := newState,
--           transitions := tfa1.transitions ++ tfa2.transitions ++ [
--           { source := newState, target := s1, symbol := mtt, guard := { clock := ⟨cc2⟩, op := .le, bound := 0 }, reset := [] },
--           { source := newState, target := s2, symbol := mtt, guard := { clock := ⟨cc2⟩, op := .le, bound := 0 }, reset := [] }
--           ],
--           acceptingStates := tfa1.acceptingStates.merge tfa2.acceptingStates
--       }
--       (tfa, newState, sc2 + 1, cc2 + 1)
--       | .or n1 n2 =>
--       let (tfa1, s1, sc1, cc1) := build n1 stateCounter clockCounter
--       let (tfa2, s2, sc2, cc2) := build n2 sc1 cc1
--       let newState := { idx := sc2 }
--       let tfa := {
--           states := tfa1.states.insert newState,
--           alphabet := tfa1.alphabet.merge tfa2.alphabet,
--           initialState := newState,
--           transitions := tfa1.transitions ++ tfa2.transitions ++ [
--           { source := newState, target := s1, symbol := mtt, guard := { clock := ⟨cc2⟩, op := .le, bound := 0 }, reset := [] },
--           { source := newState, target := s2, symbol := mtt, guard := { clock := ⟨cc2⟩, op := .le, bound := 0 }, reset := [] }
--           ],
--           acceptingStates := tfa1.acceptingStates.merge tfa2.acceptingStates
--       }
--       (tfa, newState, sc2 + 1, cc2 + 1)
--       | .until n1 w n2 => sorry
--       | .since n1 w n2 => sorry
--   build node 0 0
end Beta
