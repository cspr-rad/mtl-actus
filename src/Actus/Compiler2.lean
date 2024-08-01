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
