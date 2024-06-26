import Actus.Types.Classes
import Actus.Types.Numbers
import Actus.Types.Time
import Actus.Logic
import Actus.Automata
open MetricTemporal TimedFinite

structure ActusContract where
  terms : Type
  event : Type
  event_atomicprop : AtomicProp event
  contract : terms -> Proposition event
  automaton : terms -> TFA event

/-! # ACTUS Contracts -/
/- A contract module is of signaturea -/
/- * (Terms Event : Type) -/
/- * A metric temporal logic formula over atomic propositions Event, called contract : Terms -> MetricTemporal.Proposition Event -/
namespace PAM
  -- payment interval assumed to be 1
  structure Terms where
    principal : Money
    interest_rate : Timestamp -> Scalar -- fixed rate as constant function
    start_date : Timestamp
    maturity : TimeDelta

  inductive Event :=
  | Maturity : Event
  | PrincipalRepayment : Event
  | InterestPayment : Event
    deriving BEq, Hashable, Repr, DecidableEq

  instance event_atomicprop : AtomicProp Event := by constructor

  def Contract := Proposition Event deriving BEq, Hashable, Repr

  def contract_length (terms : Terms) : Window := (terms.start_date, terms.start_date.add_delta terms.maturity)

  def liveness (terms : Terms) : Contract :=
    let cl := contract_length terms;
    {◇ cl} [[Event.PrincipalRepayment]] and {◇ cl} [[Event.InterestPayment]] and {◇ cl} [[Event.Maturity]]

  def safety (terms : Terms) : Contract :=
    let cl := contract_length terms;
    (~ [[Event.Maturity]]) U [[Event.InterestPayment]] in cl

  def ip_continuous_till_mat (terms : Terms) : Contract :=
    let cl := contract_length terms;
    {□ cl} ([[Event.InterestPayment]] implies
        ({◯ cl.incr_start {dt := 1}} ([[Event.InterestPayment]] or [[Event.PrincipalRepayment]])))
      U [[Event.Maturity]] in cl

  def contract (terms : Terms) : Contract := ip_continuous_till_mat terms and safety terms and liveness terms

  def automaton (terms : Terms) : TFA Event := Id.run do
    let cl := contract_length terms
    let start := State.mk 0
    let interestPaid := State.mk 1
    let matured := State.mk 2
    let states := Lean.HashSet.empty
    let alphabet := Lean.HashSet.empty
    let acceptingStates := Lean.HashSet.empty

    let tfa : TFA Event := {
      states := states.insertMany [start, interestPaid, matured],
      alphabet := alphabet.insertMany [Event.InterestPayment, Event.PrincipalRepayment, Event.Maturity],
      initialState := start,
      transitions := [
        {
          source := start,
          target := interestPaid,
          symbol := Event.InterestPayment,
          guard := { clock := ClockVar.mk 0, op := GuardOp.le, bound := terms.maturity.dt.toNat },
          reset := []
        },
        {
          source := interestPaid,
          target := interestPaid,
          symbol := Event.InterestPayment,
          guard := { clock := ClockVar.mk 0, op := GuardOp.le, bound := terms.maturity.dt.toNat },
          reset := [ClockVar.mk 0]
        },
        {
          source := interestPaid,
          target := interestPaid,
          symbol := Event.PrincipalRepayment,
          guard := { clock := ClockVar.mk 0, op := GuardOp.le, bound := terms.maturity.dt.toNat },
          reset := [ClockVar.mk 0]
        },
        {
          source := interestPaid,
          target := matured,
          symbol := Event.Maturity,
          guard := { clock := ClockVar.mk 0, op := GuardOp.ge, bound := terms.maturity.dt.toNat },
          reset := []
        }
      ],
      acceptingStates := acceptingStates.insert matured
    }

    return tfa
end PAM

def PAMContract : ActusContract := { terms := PAM.Terms, event := PAM.Event, event_atomicprop := PAM.event_atomicprop, contract := PAM.contract, automaton := PAM.automaton }

namespace SWPPV

  structure Terms where
    swap_period : Window
    rate_dt : TimeDelta
    payment_dt : TimeDelta
    deriving BEq, Hashable, Repr

  inductive Event :=
  | Maturity : Event
  | FixedLegPayment : Event
  | FloatingLegPayment : Event
  | RateReset : Event
    deriving BEq, Hashable, Repr

  def Contract := Proposition Event deriving BEq, Hashable, Repr

  def periodic_event (event : Event) (interval : TimeDelta) (within : Window) : Proposition Event :=
    {□ within} {◇ (within.1, within.1.add_delta interval)} [[event]]
  -- TODO: one thing i think i don't like about this is we're doing `add_delta` to the end of the swap period..
  def contract (terms : Terms) : Contract :=
    let fixedLegPayments := periodic_event Event.FixedLegPayment terms.payment_dt terms.swap_period
    let floatingLegPayments := periodic_event Event.FloatingLegPayment terms.payment_dt terms.swap_period
    let rateResets := periodic_event Event.RateReset terms.rate_dt terms.swap_period
    let maturity := {◇ terms.swap_period} [[Event.Maturity]]
    fixedLegPayments and floatingLegPayments and rateResets and maturity

end SWPPV

namespace ANN
  structure Terms where
    principal : Money
    interest_rate : Timestamp -> Scalar
    start_date : Timestamp
    maturity : TimeDelta

  inductive Event :=
    | Maturity : Event
    | Payment : Money -> Event
    deriving BEq, Hashable, Repr

  def Contract := Proposition Event deriving BEq, Hashable, Repr

  def contract_length (terms : Terms) : Window := (terms.start_date, Timestamp.add_delta terms.start_date terms.maturity)

  def generate_payment_schedule (start : Timestamp) (frequency : TimeDelta) (maturity : TimeDelta) : List Timestamp :=
  List.range (maturity.dt.toNat / frequency.dt.toNat) |>.map
    (λ n => Timestamp.add_delta start { dt := n.toUInt64 * frequency.dt })

-- TODO: finish, plus more fleshing out.
--  def fold_payments (dates : List Timestamp) : Contract :=
--    let last_t := dates.reverse.head;
--    dates.foldr
--      (λ date acc => {□ (date, date.add_delta { dt := 1 })} [[Event.Payment {amount := 100}]] and acc)
--      ({◇ last_t.add_delta { dt := 1 }} [[Event.Maturity]])
--
--  def periodic_payments (terms : Terms) : Contract :=
--    let payment_periods := generate_payment_schedule terms.start_date terms.maturity { dt := 1 }
--    fold_payments payment_periods
end ANN
