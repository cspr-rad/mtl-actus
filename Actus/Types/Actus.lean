import Actus.Types.Numbers
import Actus.Types.Time

namespace StateType

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

end StateType

namespace EventType
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

end EventType
