import Lean.Data.HashSet
import Actus.Types

structure LTS where
  state : Type
  action : Type
  initial : List state
  transition : state -> action -> state -> Prop
  atomic_proposition : Type
  ap_termset : TermSet atomic_proposition
  labelling : state -> Lean.HashSet atomic_proposition

/-
def LTS.successor (lts : LTS) (s : lts.state) (a : lts.action) : HashSet lts.state := do
  let mut set : HashSet lts.state := {};
  for s' in lts.state do
    if lts.transition s a s' then
      set := set.insert s';
  return set
-/
