inductive TestResult
| success : TestResult
| failure : String -> TestResult

structure TestState where
  failures : Nat := 0
  messages : Array String := #[]
  successes : Array String := #[]
instance TestStateInhabited : Inhabited TestState where
  default := TestState.mk 0 #[] #[]

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

def testNamed (name : String) (body : TestM Unit) : TestM Unit := do
  let initialState <- get
  body
  let finalState <- get
  if finalState.failures == initialState.failures then
    modify fun s => { s with successes := s.successes.push name }

syntax "name_test" ident : command

macro_rules
| `(name_test $name:ident) =>
  `(def $name := testNamed $name (fun _ => _))

def assert (cond : Bool) (msg : String := "") : TestM Unit := do
  if cond then
    modify fun s => {
      successes := s.successes.push $ "Name of condition that succeeded",
    }
  else
    modify fun s => {
      failures := s.failures + 1,
      messages := if msg != "" then s.messages.push msg else s.messages,
    }

def runTest (test : TestM Unit) : IO TestResult := do
  let (_, state) ← test () |>.run {}
  if state.failures == 0 then
    IO.println "Successful tests:"
    for name in state.successes do
      IO.println s!"▸ {name}"
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
