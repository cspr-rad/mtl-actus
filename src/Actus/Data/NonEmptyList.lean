inductive NonEmptyList (α : Type u) where
  | single : α → NonEmptyList α
  | cons : α → NonEmptyList α → NonEmptyList α
  deriving Repr

namespace NonEmptyList

  def head : NonEmptyList α → α
    | single x => x
    | cons x _ => x

  def tail : NonEmptyList α → Option (NonEmptyList α)
    | single _ => none
    | cons _ xs => some xs

  def toList : NonEmptyList α → List α
    | single x => [x]
    | cons x xs => x :: xs.toList

  def length : NonEmptyList α → Nat
    | single _ => 1
    | cons _ xs => 1 + xs.length

  def append : NonEmptyList α → NonEmptyList α → NonEmptyList α
    | single x, ys => cons x ys
    | cons x xs, ys => cons x (xs.append ys)

  def map (f : α → β) : NonEmptyList α → NonEmptyList β
    | single x => single (f x)
    | cons x xs => cons (f x) (xs.map f)

  def foldr (f : α → β → β) (init : β) : NonEmptyList α → β
    | single x => f x init
    | cons x xs => f x (xs.foldr f init)

  def foldl (f : β → α → β) (init : β) : NonEmptyList α → β
    | single x => f init x
    | cons x xs => xs.foldl f (f init x)

  def reverse : NonEmptyList α → NonEmptyList α
    | single x => single x
    | cons x xs => append xs.reverse (single x)

  def ofList : List α → Option (NonEmptyList α)
    | [] => none
    | x::xs => ofList xs |>.map (cons x)

  def all (p : α → Bool) : NonEmptyList α → Bool
    | single x => p x
    | cons x xs => p x && xs.all p

  def any (p : α → Bool) : NonEmptyList α → Bool
    | single x => p x
    | cons x xs => p x || xs.any p

  instance [BEq α] : BEq (NonEmptyList α) where
    beq xs ys :=
      let rec beq' : NonEmptyList α → NonEmptyList α → Bool
        | single x, single y => x == y
        | cons x xs, cons y ys => x == y && (beq' xs ys)
        | _, _ => false
      beq' xs ys

  instance [Hashable α] : Hashable (NonEmptyList α) where
    hash x :=
      let rec hash' : NonEmptyList α → UInt64
        | single x => hash x
        | cons x xs => hash (hash x + hash' xs)
      hash' x
  syntax "{[" term,+ "]}" : term

  macro_rules
    | `({[ $x ]}) => `(NonEmptyList.single $x)
    | `({[ $x, $xs:term,* ]}) => `(NonEmptyList.cons $x (NonEmptyList.ofList! [$xs,*]))

  infix:65 " ++ " => NonEmptyList.append

end NonEmptyList

