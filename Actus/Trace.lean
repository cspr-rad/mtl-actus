import Actus.Types

namespace Trace
  structure TraceEntry (E : Type) where
    e : E
    t : Timestamp

  def T (E : Type) := E |> TraceEntry |> List
end Trace
