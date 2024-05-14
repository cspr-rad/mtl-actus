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
#let title = "Formally Verify Finance like a Reactive System"

#show: university-theme.with(short-title: title, short-author: "Quinn Dougherty", short-date: today)

#title-slide(
  title: title, subtitle: "Lessons from Model Checking", authors: [Quinn Dougherty], institution-name: "Casper Association", date: today, logo: image("./cspr-favicon.png"),
)

#slide(title: [Automata])[
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
#renderthreequarters(vending_machine)
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
  title: [Reactive systems],
)[
  - A *reactive system* is a software system embedded in an environment that
    responds to sensor input
    - often in continuous/infinite time horizon
    - often with actuator output effecting the environment
  #uncover(2)[
  - Examples: traffic lights, airplane autopilot, fitness tracker on smartwatch, cruise control on a car
  ]
    #figure(image("cash_flows.png", width: 35%))
]

#slide(
    title: [Temporal Logic],
)[
    - Logic aware of time step
    - We can *specify* correctness of a financial contract as well as safety and liveness properties
    #figure(image("cash_flows.png", width: 35%))
    $#"PAM" : diamond.stroked #"Mat" and not #"Mat" #"U" #"IP" and square.stroked (#"IP" -> (circle.stroked (#"IP" or #"PR")) #"U" #"Mat")$
]
