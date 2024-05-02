#import "@preview/polylux:0.3.1": *
#import "@preview/diagraph:0.2.0": *
#import "@preview/cetz:0.2.2": *
#import themes.university: *
#let renderc(code) = render(code.text)

#let today = datetime.today().display()
#let title = "Toward Formally Verified Finance"

#show: university-theme.with(
  short-title: title,
  short-author: "Quinn Dougherty",
  short-date: today,
)

#title-slide(
  title: title,
  subtitle: "Lessons from Model Checking",
  authors: [Quinn Dougherty],
  institution-name: "Casper Association",
  date: today,
  logo: image("./cspr-favicon.png"),
)

#slide()[
  #utils.polylux-outline()
]

#slide(title: [Overview])[
  - ACTUS: finance down to state machine abstraction
  - Temporal logic: specify behaviors with time
  - Automata: execution abiding by temporal logic specifications
]

#slide(title: [Algorithmic Contract Types Unified Standard], new-section: [ACTUS])[ \
  ACTUS simulates _cash flows_ with a _state machine_ abstraction. \
]

#slide(title: [ACTUS state machine])[
  _writes about the actus state machines_
]

#slide(title: [], new-section: [Logic and time])[
  _writes about logic_
]

#slide(title: [Automata theory for finance], new-section: [Automata])[
  == It turns out
  ACTUS' notion of state machine is a special case of what's studied in _automata theory_
]

#slide(title: [Automata theory for finance])[
  An automaton is an abstraction of computation consisting of states connected by events
  - #uncover((2,3,4))[In a _finite_ automaton, some states are distinguished as "final"]
  - #uncover((3,4))[In a _timed_ automaton, transitions (traversing along events) increment some "clocks", and events can only fire if "guard conditions" on those clocks are met]
  - #uncover(4)[In our case, events are _labeled_]
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
      s0 -> s1 [label="label 1"]
      s1 -> s1 [label="label 2"]
      s1 -> s2 [label="label 3"]
    }
  ```)
]

#slide(title: [Principal at Maturity (PAM)])[
  #renderc(```
    digraph pam_automaton {
      rankdir=LR

      node [shape=circle]
      preinit [label="", shape=none]
      start
      int_pmnt
      node [shape=doublecircle]
      maturity

      preinit -> start
      start -> int_pmnt [label="IP; c < m"]
      int_pmnt -> int_pmnt [label="IP; c < m"]
      int_pmnt -> maturity [label="PR; c > m"]
    }
  ```)
  - c: _clock_
  - m: _maturity date_
  - IP: interest payment event
  - PR: principal repayment event
  - All events increment clock c by 1
]

#slide(title: [], new-section: [Model checking])[

]

#matrix-slide[
  left
][
  middle
][
  right
]

#matrix-slide(columns: 1)[
  top
][
  bottom
]

#matrix-slide(columns: (1fr, 2fr, 1fr), ..(lorem(8),) * 9)

#slide(title: [notes (not actually gonna be a slide)], new-section: [TODO: delete])[
  - it'd be good to have a section on the different usecases of formal verification, like modeling vs verification of prod code
  - unpack what model checking is
]
