import Actus.Types

namespace Trace
  structure TraceEntry (E : Type) where
    e : E
    t : Time.Timestamp

  def T (E : Type) := E |> TraceEntry |> List
end Trace
