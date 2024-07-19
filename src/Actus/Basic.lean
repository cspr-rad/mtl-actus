-- import Batteries.Data.List.Basic
import Actus.Contracts
import Actus.Automata

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

namespace UnitTest
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
  instance PAMInhabited : Inhabited (@Execution.TimedLetter PAM.Event) where
    default := { symbol := PAM.Event.InterestPayment, time := FiniteTimestamp.mk 0 }

  def somePamWord : @Execution.TimedWord PAM.Event :=
    let ip1 := { symbol := PAM.Event.InterestPayment, time := FiniteTimestamp.mk 1 };
    let ip2 := { symbol := PAM.Event.InterestPayment, time := FiniteTimestamp.mk 2 };
    let pr := { symbol := PAM.Event.PrincipalRepayment, time := FiniteTimestamp.mk 3 };
    let m := { symbol := PAM.Event.Maturity, time := FiniteTimestamp.mk 10 };
    let letters := [ip1, ip2, pr, m];
    let nonDecreasing : Execution.isNonDecreasing letters := by
      intros i j H0 H1;
      have H2 : i < letters.length := Nat.lt_trans H1 H0;
      have H3 : letters[i] ≤ letters[j] := by
        match i, j with
        | 0, 0 | 1,0 | 1,1 | 2,0 | 2,1 | 2,2 | 3,0 | 3,1 | 3,2 | 3,3 => contradiction
        | 0, 1 =>
          show ip1 <= ip2
          simp [Execution.TimedLetterLE, FiniteTimestampLE]
        | 0, 2 =>
          show ip1 <= pr
          simp [Execution.TimedLetterLE, FiniteTimestampLE]
        | 0, 3 =>
          show ip1 <= m
          simp [Execution.TimedLetterLE, FiniteTimestampLE]
        | 1, 2 =>
          show ip2 <= pr
          simp [Execution.TimedLetterLE, FiniteTimestampLE]
        | 1, 3 =>
          show ip2 <= m
          simp [Execution.TimedLetterLE, FiniteTimestampLE]
        | 2, 3 =>
          show pr <= m
          simp [Execution.TimedLetterLE, FiniteTimestampLE]
        | i' + 4, _ => simp [letters] at H0; omega
        | _, j' + 4 => simp [letters] at H0; omega
      exact H3;
    Execution.TimedWord.mk letters nonDecreasing

  def emptyWord : @Execution.TimedWord PAM.Event :=
    let letters := [];
    let nonDecreasing : Execution.isNonDecreasing letters := by sorry
    Execution.TimedWord.mk letters nonDecreasing

  #eval (PAM.automaton somePam).accepts _ somePamWord
  -- #eval (PAM.automaton somePam).accepts _ emptyWord
  -- #eval transform_swppv_contract <| SWPPV.contract <| someSwppv
  -- #eval PAM.automaton.accepts
end UnitTest
