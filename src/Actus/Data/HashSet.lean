import Lean.Data.HashSet

def Lean.HashSet.intersect {α} [BEq α] [Hashable α] (s1 s2 : HashSet α) : HashSet α := Id.run do
  let mut result := HashSet.empty
  for x in s1 do
    if s2.contains x then
      result := result.insert x
  return result
