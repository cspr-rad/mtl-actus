import Actus

def emptyWord (α : Type) : @Execution.TimedWord α :=
  let letters: List (@Execution.TimedLetter α) := []
  let nonDecreasing : Execution.isNonDecreasing letters := by sorry;
  { letters, nonDecreasing }
