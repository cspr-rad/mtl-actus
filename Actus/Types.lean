class TermSet (T : Type) extends BEq T, Hashable T

structure Party where
  name : String
  balance : Int -- Money
  deriving BEq, Hashable, Repr

def Execution : Type -> Type := IO deriving Functor, Applicative, Monad

structure Money where
  amount : Int
  deriving BEq, Hashable, Repr

def Money.map (f : Int -> Int) (m : Money) : Money :=
  { amount := f m.amount }

structure Scalar where
  value : Int
  deriving BEq, Hashable, Repr

def Scalar.map (f : Int -> Int) (s : Scalar) : Scalar :=
  { value := f s.value }

inductive Timestamp : Type where
  | t : UInt64 -> Timestamp
  | infinity : Timestamp
  deriving BEq, Hashable, Repr

def Timestamp.map (f : UInt64 -> UInt64) (t : Timestamp) : Timestamp :=
  match t with
  | Timestamp.t time => Timestamp.t (f time)
  | Timestamp.infinity => Timestamp.infinity

-- Implementation that returns true if t1 is before t2
def Timestamp.before (t1 t2 : Timestamp) : Bool := match t1, t2 with
  | Timestamp.t time1, Timestamp.t time2 => time1 < time2
  | Timestamp.infinity, _ => false
  | _, Timestamp.infinity => true

structure TimeDelta where
  dt : UInt64
  deriving BEq, Hashable, Repr

def TimeDelta.map (f : UInt64 -> UInt64) (td : TimeDelta) : TimeDelta :=
  { dt := f td.dt }

def Timestamp.add_delta (t1 : Timestamp) (t2 : TimeDelta): Timestamp :=
  match t1 with
  | Timestamp.t time1 => Timestamp.t (time1 + t2.dt)
  | _ => Timestamp.infinity

namespace Interval
  def T : Type := (Timestamp × Timestamp)
  deriving  BEq, Hashable, Repr

  def T.map (f : Timestamp -> Timestamp) (i : T) : T :=
    (f i.1, f i.2)

  def T.incr_start (i : T) (d : TimeDelta) : T :=
    (Timestamp.add_delta i.1 d, i.2)

  -- todo: contains
  -- todo: subinterval
  -- todo: intersection, probably
end Interval

def Window : Type := Interval.T deriving BEq, Hashable, Repr

namespace State

  inductive ContractPerformance : Type :=
  | performant
  | delayed
  | delinquent
  | default
  | matured
  | terminated

  inductive T : Type :=
  | accruedInterest : Money -> T
  | accruedInterest2 : Money -> T
  | boundaryCrossedFlag : Bool -> T
  | boundaryLeg1ActiveFlag : Bool -> T
  | boundaryLeg2ActiveFlag : Bool -> T
  | contractPerformance : ContractPerformance -> T
  | exerciseAmount : Money -> T
  | exerciseDate : Timestamp -> T
  | feeAccrued : Money -> T
  | interestCalculationBaseAmount : Money -> T
  | interestScalingMultiplier : Scalar -> T
  | maturityDate : Timestamp -> T
  | nextPrincipalRedemptionPayment : Money -> T
  | nominalInterestRate : Money -> T
  | nominalInterestRate2 : Money -> T
  | nonPerformingDate : Timestamp -> T
  | notionalPrincipal : Money -> T
  | notionalPrincipal2 : Money -> T
  | notionalScalingMultiplier : Money -> T
  | statusDate : Timestamp -> T
  | terminationDate : Timestamp -> T

end State

namespace Event
  inductive T : Type :=
  | monitoring
  | initialExchange
  | feePayment
  | principalRedemption
  | principalDrawing
  | principalPaymentAmountFixing
  | penaltyPayment
  | principalPrepayment
  | interestPayment
  | interestCapitalization
  | creditEvent
  | rateResetFixed
  | rateResetVariable
  | dividendPayment
  | purchase
  | marginCall
  | termination
  | scalingIndexFixing
  | interestCalculationBaseFixing
  | maturity
  | exercise
  | settlement
  | boundaryMonitor
  | boundary

end Event
