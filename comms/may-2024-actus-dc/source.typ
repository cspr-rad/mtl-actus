#import "@preview/polylux:0.3.1": *
#import "@preview/diagraph:0.2.2": *
#import "@preview/cetz:0.2.2": *
#import "@preview/finite:0.3.0": *
#import themes.university: *
#let renderc(code) = render(code.text)
#let renderquarter(code) = render(code.text, height: 25%)
#let renderhalf(code) = render(code.text, height: 50%)
#let renderthreequarters(code) = render(code.text, height: 75%)

#let today = datetime.today().display()
#let title = "Toward Formally Verified Finance"

#show: university-theme.with(short-title: title, short-author: "Quinn Dougherty", short-date: today)

#title-slide(
  title: title, subtitle: "Lessons from Model Checking", authors: [Quinn Dougherty], institution-name: "Casper Association", date: today, logo: image("./cspr-favicon.png"),
)

#slide()[
  #utils.polylux-outline()
]

#slide(title: [Overview])[
  - ACTUS: finance down to state machine abstraction
  - Formal verification: way better quality assurance
  - Temporal logic: specify behaviors with time
  - Automata: execution abiding by temporal logic specifications
  - Reactive systems and model checking
]

#slide(
  title: [Algorithmic Contract Types Unified Standard], new-section: [ACTUS],
)[ \
    ACTUS simulates _cash flows_ with a _state machine_ abstraction. \
    #figure(image("cash_flows.png", width: 65%))
]

#slide(title: [ACTUS state machine])[
    Each contract comes equipped with
    - $T : S times E -> S$
    - $P : E -> RR$

    where
    $T$ := Transition;
    $P$ := Payoff;
    $S$ := State;
    $E$ := Event

    #only(2)[When you evaluate a state at an event $e$, a counterparty receives $P(e)$ payout (may be negative)]
]

#slide(
  title: [The study of argument structure], new-section: [Logic and quality assurance],
)[
  == A logical #underline[formula] is some propositions connected by the operators
  - To "prove" a formula is to convince a skeptic that the formula is "true" or "valid"
  - Formulae are types, proofs are programs
]

#matrix-slide(
  title: [The study of argument structure],
)[
  == Modus ponens
  #only(
    1,
  )[If it is raining, then the ground is wet. It is raining. Therefore, the ground
    is wet]
  #only(2)[$(p -> q and p) -> q$]
][
  == Modus tollens
  #only(
    1,
  )[If it is snowing, then it is cold outside. It is not cold outside. Therefore, it
    is not snowing]
  #only(2)[$(p -> q and not q) -> not p$]
]
#slide(
  title: [Formal Verification],
)[
  == Logic can set itself on any kind of mathematical object or phenomenon
  == When we use logic to study software, we're doing "formal verification"
  == Formal verification is kin with ordinary software testing, but much stronger
  Providing steeper assurances
]

#slide(title: [Baby testing: units])[
Come up with the cases you have time to enumerate
```python
  def is_even(x: int) -> bool:
    ...

  assert is_even(2)
  assert is_even(4)
  ```
]
#slide(title: [Intermediate testing: properties])[
Procedurally generate unit tests (100-10000 cases)
```python
  def is_even(x: int) -> bool:
    if x == 2: return True
    elif x == 4: return True
    else: return False

  @hypothesis.given(hypothesis.strategies.integers())
  def is_even_agrees(x: int) -> bool:
    is_even(x) == (x % 2 == 0)
  # Test fails with generated counterexample x = 6
  ```
]
#slide(
  title: [Ascended testing: formal verification],
)[\
  A type checker in a total language can *exploit the structure* of datatypes and
  functions to prove over a whole type without exhaustively checking every value ]

#slide(
  title: [Truth when?], new-section: [Logic and time],
)[
  We can make logical formulae specify properties involving time by introducing
  new operators called _modal operators_
  - $square.stroked p$ := always $p$
  - $diamond.stroked p$ := eventually $p$
  - $p U q$ := $p$ until $q$
]

#slide(title: [Automata], new-section: [Automata])[
#let vending_machine = ```
 digraph VendingMachine {
  rankdir=LR;
  node [shape = circle];

  // States
  Idle [shape = doublecircle];
  Retrieving;
  Dispensing;

  // Transitions
  Idle -> Retrieving [label = "SelectProduct"];
  Idle -> Idle [label = "InsertCoin"];
  Retrieving -> Dispensing;

  Dispensing -> Idle [label = "ProductDispensed"];
}
```
        #figure(
  image("vending_machine.jpg", width: 30%),
)
    #renderthreequarters(vending_machine)


]

#slide(
  title: [Automata theory for finance],
)[
  #uncover(
    (2, 3, 4),
  )[
    == It turns out
    ACTUS' notion of state machine is a special case of what's studied in _automata theory_
  ]
  #uncover((3, 4))[
    == Automata form semantics for temporal formulae
  ]
  #uncover(4)[
    == Automata provide the execution environment for model checking
  ]
]

#slide(
  title: [Automata theory for finance],
)[
An automaton is an abstraction of computation consisting of states connected by
events
#uncover(
  (2, 3, 4),
)[- In a _finite_ automaton, some states are distinguished as "final"]
#uncover(
  (3, 4),
)[- In a _timed_ automaton, transitions (traversing along events) increment some "clocks",
    and events can only fire if "guard conditions" on those clocks are met]
#uncover(4)[- In our case, the _guard conditions_ are _event labels_]
#renderc(```
    digraph example_automaton {
      rankdir=LR

      node [shape=circle]
      preinit [label="", shape=none]
      s0
      s1
      node [shape=doublecircle]
      s2

      preinit -> s0
      s0 -> s1 [label="label1; x<y"]
      s1 -> s1 [label="label2; x<y"]
      s1 -> s2 [label="label3; x<y"]
    }
  ```)
]

#let pam_automaton = ```
  digraph pam_automaton {
    rankdir=LR

    node [shape=circle]
    preinit [label="", shape=none]
    start
    int_pmnt
    node [shape=doublecircle]
    maturity

    preinit -> start [label="INIT"]
    start -> int_pmnt [label="IP; c < m"]
    int_pmnt -> int_pmnt [label="IP; c < m"]
    int_pmnt -> maturity [label="PR; c >= m"]
  }
```

#slide(title: [Principal at Maturity (PAM)])[
  #renderc(pam_automaton)
  - c: _clock_
  - m: _maturity date_
  - IP: interest payment event
  - PR: principal repayment event
  - All events increment clock c by 1
]

#slide(
  title: [Principal At Maturity (PAM)],
)[
#renderquarter(pam_automaton)
We can _run_ PAM as a timed finite automaton to elicit a _trace_
== The trace (for m = 2):
#only(2)[
1. Enter contract at `start` state
  - c = 0
  - push `INIT` to trace
]
#only(
  3,
)[
2. Apply an interest payment with the `IP` event, evaluating the guard $0 lt 2$ to
  enter `int_pmnt` state
  - c = 1
  - push `IP` to trace
]
#only(
  4,
)[
3. Apply an interest payment with the `IP` event, evaluating the guard $1 lt 2$ to
  enter `int_pmnt` state
  - c = 2
  - push `IP` to trace
]
#only(
  5,
)[
4. $2 lt.not 2$, but $2 gt.eq 2$, so we take the `PR` (principal repayment) event
  instead, entering the `maturity` state which is final
  - c = 3 (doesn't matter)
  - push `PR` to trace
]
#only(1)[
  0. Start with empty trace

  []
]
#only(2)[Result: [#text(blue)[`INIT`]]]
#only(3)[Result: [`INIT`, #text(blue)[`IP`]]]
#only(4)[Result: [`INIT`, `IP`, #text(blue)[`IP`]]]
#only(5)[So the run produces trace: [`INIT`, `IP`, `IP`, #text(blue)[`PR`]]]
]

#slide(
  title: [Reactive systems], new-section: [Model checking],
)[
  - A *reactive system* is a software system embedded in an environment that
    responds to sensor input
    - often in continuous/infinite time horizon
    - often with actuator output effecting the environment
]

#slide(
  title: [Formal verification for reactive systems],
)[\
  // inserts traffic light picture
  // maybe more diagrams
  #uncover(
    (1, 2, 3),
  )[== Recall that formal verification deals in mathematical proofs of software correctness]
  #uncover(
    (2, 3),
  )[- A temporal logic forms a *specification language* in which normative constraints
      for reactive systems can be captured]
  #uncover(
    3,
  )[
    - *Model checking* is the discipline of turning programs into automata and showing
      that the automata is validated by a spec
      - Two key types of properties are _safety_ properties and _liveness_ properties
  ]
]

#matrix-slide[
  == Safety
  _nothing bad ever happens_
][
  == Liveness
  _something good eventually happens_
]

#slide(
  title: [notes (not actually gonna be a slide)], new-section: [TODO: delete],
)[
  - it'd be good to have a section on the different usecases of formal verification,
    like modeling vs verification of prod code
  - unpack what model checking is
]

#matrix-slide(columns: 1)[
  top
][
  bottom
]

#matrix-slide(columns: (1fr, 2fr, 1fr), ..(lorem(8),) * 9)
