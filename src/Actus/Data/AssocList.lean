import Lean.Data.AssocList

def Lean.AssocList.insertOrReplace
    {α β}
    [BEq α] [Hashable α]
    (m : AssocList α β) (k : α) (v : β) : AssocList α β :=
  match m.find? k with
  | some _ => m.replace k v
  | none   => m.insert k v
