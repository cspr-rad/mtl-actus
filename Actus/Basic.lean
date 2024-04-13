import Actus.Trace
import Actus.Contracts
open Time

def PAMTrace := Trace.T PAM.Event
def SWPPVTrace := Trace.T SWPPV.Event

-- Fixed leg payment version
def pam_swppv_event_map (e : PAM.Event) : SWPPV.Event :=
  match e with
  | PAM.Event.Maturity => SWPPV.Event.Maturity
  | PAM.Event.PrincipalRepayment => SWPPV.Event.RateReset
  | PAM.Event.InterestPayment => SWPPV.Event.FixedLegPayment
def pam_swppv_event_lift (e : PAM.Event) : SWPPV.Contract :=
  [[pam_swppv_event_map e]]
def transform_pam_contract (p : PAM.Contract) : SWPPV.Contract :=
  p >>= pam_swppv_event_lift

def swppv_pam_event_map (e : SWPPV.Event) : PAM.Event :=
  match e with
  | SWPPV.Event.Maturity => PAM.Event.Maturity
  | SWPPV.Event.RateReset => PAM.Event.PrincipalRepayment
  | SWPPV.Event.FixedLegPayment => PAM.Event.InterestPayment
  | SWPPV.Event.FloatingLegPayment => PAM.Event.InterestPayment
def swppv_pam_event_lift (e : SWPPV.Event) : PAM.Contract :=
  [[swppv_pam_event_map e]]
def transform_swppv_contract (p : SWPPV.Contract) : PAM.Contract :=
  p >>= swppv_pam_event_lift

namespace Test
  def somePam : PAM.Terms := {
    principal := { amount := 100 },
    interest_rate := fun _ => { value := 1 },
    start_date := Timestamp.t 0,
    maturity := { dt := 10 }
  }

  def someSwppv : SWPPV.Terms := {
    swap_period := (Timestamp.t 0, Timestamp.t 10),
    rate_dt := { dt := 1 },
    payment_dt := { dt := 1 },
  }

  #eval transform_swppv_contract <| SWPPV.contract <| someSwppv

  -- #eval contract someTerms
end Test
-- TODO: https://chat.openai.com/c/f756902c-9ce3-4c76-b1bc-9ef47716103c pick up here with:
-- - composing pam with swap
-- - "trace" executions and their validation

def hello := "world"
