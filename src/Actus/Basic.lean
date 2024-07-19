import Actus.Contracts
import Actus.Automata

-- Sketch: use monadic bind to pipe swap and pam into a composed contract.
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
