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

inductive NegationNormalForm : Proposition T -> Prop where
| mtt : NegationNormalForm mtt
| atom (x : T) : NegationNormalForm [[x]]
| neg_atom (x : T) : NegationNormalForm (~ [[x]])
| and (φ ψ : Proposition T) : NegationNormalForm φ -> NegationNormalForm ψ -> NegationNormalForm (φ and ψ)
| until (φ : Proposition T) (w : Window) (ψ : Proposition T) : NegationNormalForm φ ->
    NegationNormalForm ψ ->
    NegationNormalForm (φ U ψ in w)
| since (φ : Proposition T) (w : Window) (ψ : Proposition T) : NegationNormalForm φ ->
    NegationNormalForm ψ ->
    NegationNormalForm (φ S ψ in w)

def Proposition.isNNF (φ : Proposition T) : Bool :=
  match φ with
  | mtt => true
  | [[x]] => true
  | ~ [[x]] => true
  | ~ φ' => false
  | φ' and ψ' => isNNF φ' && isNNF ψ'
  | φ' U ψ' in w => isNNF φ' && isNNF ψ'
  | φ' S ψ' in w => isNNF φ' && isNNF ψ'

theorem Proposition.isNNF_iff (φ : Proposition T) : isNNF φ = true ↔ NegationNormalForm φ := by
  apply Iff.intro <;> intro H
  · induction φ
    · constructor
    · constructor
    · sorry
    · simp [isNNF] at H
      rcases H with ⟨H1, H2⟩
      -- specialize a_ih1 H1
      -- specialize a_ih H2
      -- constructor
      sorry
    · sorry
    · sorry
  · induction H <;> simp [isNNF] <;> constructor <;> assumption

instance DecidableNNF (φ : Proposition T) (H : NegationNormalForm φ) : Decidable (NegationNormalForm φ) := by
  apply decidable_of_iff (Proposition.isNNF φ = true) (Proposition.isNNF_iff φ)

inductive Closure (φ : Proposition T) (H : NegationNormalForm φ): Proposition T → Prop where
  | init : Closure φ H φ
  | subformula_until_left {ψ χ : Proposition T} {w : Window} :
      Closure φ H (ψ U χ in w) → NegationNormalForm ψ → NegationNormalForm χ →
      Closure φ H ψ
  | subformula_until_right {ψ χ : Proposition T} {w : Window} :
     Closure φ H (ψ U χ in w) → NegationNormalForm ψ → NegationNormalForm χ →
      Closure φ H χ
  | subformula_since_left {ψ χ : Proposition T} {w : Window} :
      Closure φ H (ψ S χ in w) → NegationNormalForm ψ → NegationNormalForm χ →
      Closure φ H ψ
  | subformula_since_right {ψ χ : Proposition T} {w : Window} :
      Closure φ H (ψ S χ in w) → NegationNormalForm ψ → NegationNormalForm χ →
      Closure φ H χ
  | residual_next {ψ : Proposition T} {w : Window} :
      Closure φ H ({◯ w} ψ) → NegationNormalForm ψ → Closure φ H ({◯ w} ψ)

def Proposition.closure (φ : Proposition T) (H : NegationNormalForm φ) : Lean.HashSet (Proposition T) :=
  let acc := Lean.HashSet.empty.insert φ
  let rec go (acc : Lean.HashSet (Proposition T)) (φ : Proposition T) : Lean.HashSet (Proposition T) :=
    match φ with
    | mtt => acc
    | [[x]] => acc
    | ~ φ' => go acc φ'
    | φ' and ψ' => go (go acc φ') ψ'
    | φ' U ψ' in w => go (go acc φ') ψ'
    | φ' S ψ' in w => go (go acc φ') ψ'
  go acc φ

-- TODO: closure correct
theorem Closure_correct (φ : Proposition T) (H : NegationNormalForm φ) :
    ∀ ψ, Closure φ H ψ ↔ (Proposition.closure φ H).contains ψ :=
    by sorry
