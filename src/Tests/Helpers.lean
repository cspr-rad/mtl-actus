import Actus

def mkTimedLetter (symbol : α) (time : Nat) : @Execution.TimedLetter α :=
  { symbol, time := FiniteTimestamp.mk time }

def mkTimedWord (letters : List (@Execution.TimedLetter α)) : @Execution.TimedWord α :=
  { letters, nonDecreasing := sorry }
