import Lean.Data.HashSet
import Actus.Types
open Time
/-!
Our MTL admits discrete time intervals.
-/
namespace MetricTemporal
  variable {T : Type} [AtomicProp T]

  inductive Proposition (T : Type) : Type where
  | mt_t : Proposition T
  | atom : T -> Proposition T
  | negate : Proposition T -> Proposition T
  | conjunct : Proposition T -> Proposition T -> Proposition T
  | until : Proposition T -> Window -> Proposition T -> Proposition T
  | since : Proposition T -> Window -> Proposition T -> Proposition T
  deriving Hashable, BEq, Repr

  instance : Inhabited (Proposition T) where
    default := Proposition.negate Proposition.mt_t

  notation "mtt" => Proposition.mt_t
  notation "[[" x "]]" => Proposition.atom x
  notation "~" φ => Proposition.negate φ
  notation "mtf" => ~ mtt
  notation φ "and" ψ => Proposition.conjunct φ ψ
  notation φ "U" ψ "in" w => Proposition.until φ w ψ
  notation φ "S" ψ "in" w => Proposition.since φ w ψ

  def or (φ ψ : Proposition T) : Proposition T := ~ (~ φ and ~ ψ)
  def implies (φ ψ : Proposition T) : Proposition T := or (~ φ) ψ
  def eventually (w : Window) (φ : Proposition T) : Proposition T := mtt U φ in w
  def always (w : Window) (φ : Proposition T) : Proposition T := ~ (eventually w (~ φ))
  def release (w : Window) (φ ψ : Proposition T) : Proposition T := ~ ((~ φ) U (~ ψ) in w)
  def trigger (w : Window) (φ ψ : Proposition T) : Proposition T := release w ψ φ
  def next (w : Window) (φ : Proposition T) : Proposition T := mtf U φ in w
  def previously (w : Window) (φ : Proposition T) : Proposition T := mtf S φ in w

  notation φ "or" ψ => or φ ψ
  notation φ "implies" ψ => implies φ ψ
  notation "{◇" w "}" φ => eventually w φ
  notation "{□" w "}" φ => always w φ
  notation "{◯" w "}" φ => next w φ
  notation "◇" φ => {◇ (Timestamp.t 0, Timestamp.t 0)} φ
  notation "□" φ => {□ (Timestamp.t 0, Timestamp.t 0)} φ
  notation "{◇◇" w "}" φ => {◇ w} {◇ w} φ
  notation "{□□" w "}" φ => {□ w} {□ w} φ
  notation "{◇□" w "}" φ => {◇ w} {□ w} φ
  notation "{□◇" w "}" φ => {□ w} {◇ w} φ
  def iff (φ ψ : Proposition T) : Proposition T := (φ implies ψ) and (ψ implies φ)
  notation φ "iff" ψ => iff φ ψ

  def Proposition.map (f : α -> β) : Proposition α -> Proposition β
  | mtt => mtt
  | [[x]] => [[f x]]
  | ~ φ => ~ (map f φ)
  | φ and ψ => (map f φ) and (map f ψ)
  | φ U ψ in w => (map f φ) U (map f ψ) in w
  | φ S ψ in w => (map f φ) S (map f ψ) in w

  instance : Functor Proposition where
    map := Proposition.map

  def Proposition.seq (f : Proposition (α → β)) (x : Unit → Proposition α) : Proposition β :=
    match f with
    | mtt => mtt
    | [[f]] => f <$> x ()
    | ~ p => ~ (seq p x)
    | p and q => (seq p x) and (seq q x)
    | p U q in w => (seq p x) U (seq q x) in w
    | p S q in w => (seq p x) S (seq q x) in w

  instance : Applicative Proposition where
    pure := Proposition.atom
    seq := Proposition.seq

  def Proposition.bind (x : Proposition α) (f : α -> Proposition β) : Proposition β :=
    match x with
    | mtt => mtt
    | [[x]] => f x
    | ~ p => ~ (bind p f)
    | p and q => (bind p f) and (bind q f)
    | p U q in w => (bind p f) U (bind q f) in w
    | p S q in w => (bind p f) S (bind q f) in w

  instance : Monad Proposition where
    bind := Proposition.bind

end MetricTemporal

namespace MetricTemporalSemantics
  open MetricTemporal Time
  variable (T : Type) [AtomicProp T]

  def Model (T : Type) : Type := Timestamp -> T

  def holds (γ : Model T) (t : TimeDelta) (φ : Proposition T) : Prop :=
    match φ with
    | mtt => True
    | [[x]] => x = γ t.toTimestamp
    | ~ φ => ¬ holds γ t φ
    | φ and ψ => holds γ t φ ∧ holds γ t ψ
    | φ U ψ in w => ∃ u, w.incr t |>.contains u.toTimestamp -> holds γ u ψ ∧ forall v, u < v ∧ v < t -> holds γ v φ
    | φ S ψ in w => ∃ u, w.decr t |>.contains u.toTimestamp -> holds γ u ψ ∧ forall v, u < v ∧ v < t -> holds γ v φ

  -- notation γ ";" t "⊨" φ => holds γ t φ -- leads to mysterious bugs

end MetricTemporalSemantics
