inductive TestResult
| success : TestResult
| failure : String -> TestResult

structure TestState where
  failures : Nat := 0
  messages : Array String := #[]
instance TestStateInhabited : Inhabited TestState where
  default := TestState.mk 0 #[]

def TestM : Type -> Type := ReaderT Unit (StateT TestState IO)
instance TestMMonad : Monad TestM where
  pure := fun a => fun _ s => pure (a, s)
  bind ma f := fun r s => do
    let (a, s') <- ma r s
    f a r s'
instance TestMMonadState : MonadState TestState TestM where
  get := fun _ s => pure (s, s)
  set := fun new_s _ _ => pure ((), new_s)
  modifyGet := fun f _ s =>
    let (a, s') := f s
    pure (a, s')

def assert (cond : Bool) (msg : String := "") : TestM Unit := do
  if cond then
    pure ()
  else
    modify fun s => {
      failures := s.failures + 1,
      messages := s.messages.push msg
    }

def runTest (test : TestM Unit) : IO TestResult := do
  let (_, state) ← test () |>.run {}
  if state.failures == 0 then
    pure TestResult.success
  else
    IO.println "Test failures:"
    for msg in state.messages do
      IO.println s!"  {msg}"
    pure (TestResult.failure s!"Test failed at {state.failures} assertion(s)")

def printTestRun (test : TestM Unit) : IO Unit := do
  let result <- runTest test
  match result with
  | .success => IO.println "SUCCESS!"
  | .failure msg => IO.println s!"FAILURE! {msg}"
