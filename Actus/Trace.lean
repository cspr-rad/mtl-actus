import Actus.Types
import Actus.Logic
open MetricTemporalSemantics

structure TraceEntry (E : Type) where
  e : E
  t : Time.Timestamp

def TraceEntry.map {E F : Type} (f : E -> F) (te : TraceEntry E) : TraceEntry F :=
  { e := f te.e, t := te.t }

instance : Functor TraceEntry where
  map := TraceEntry.map

def Trace (E : Type) := E |> TraceEntry |> List

def Trace.toModel (trace : Trace E) : Model E :=
  fun t => trace.find? (fun entry => entry.t = t) |>.map (·.e)

-- def Trace.valid (E : Type) (trace : Trace E) (contract : MetricTemporal.Proposition E): Prop :=
