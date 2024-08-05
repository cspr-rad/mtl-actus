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
| until (φ : Proposition T) (w : Window) (ψ : Proposition T) : NegationNormalForm φ -> NegationNormalForm ψ -> NegationNormalForm (φ U ψ in w)
| since (φ : Proposition T) (w : Window) (ψ : Proposition T) : NegationNormalForm φ -> NegationNormalForm ψ -> NegationNormalForm (φ S ψ in w)

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

/-
def Proposition.closure (φ : Proposition T) (H : NegationNormalForm φ) : Lean.HashSet (Proposition T) :=
  let rec buildClosure (set : Lean.HashSet (Proposition T)) : Lean.HashSet (Proposition T) :=
    let newSet := set.fold (fun acc ψ =>
      match ψ with
      | ψ₁ U ψ₂ in w =>
          if NegationNormalForm ψ₁ ∧ NegationNormalForm ψ₂ then
            acc.insert ψ₁
               .insert ψ₂
          else acc
      | ψ₁ S ψ₂ in w =>
          if NegationNormalForm ψ₁ ∧ NegationNormalForm ψ₂ then
            acc.insert ψ₁
               .insert ψ₂
          else acc
      | {◯ w} ψ₁ =>
          if NegationNormalForm ψ₁ then
            acc.insert ({◯ w} ψ₁)
          else acc
      | _ => acc
    ) set
    if newSet.size > set.size then buildClosure newSet else newSet

  buildClosure (Lean.HashSet.empty.insert φ)
-/
