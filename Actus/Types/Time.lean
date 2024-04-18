inductive Timestamp : Type where
  | t : UInt64 -> Timestamp
  | infinity : Timestamp
  deriving BEq, Hashable, Repr, DecidableEq

instance : Ord Timestamp where
  compare t1 t2 := match t1, t2 with
    | Timestamp.t time1, Timestamp.t time2 => if time1 < time2 then Ordering.lt else if time1 > time2 then Ordering.gt else Ordering.eq
    | Timestamp.infinity, Timestamp.infinity => Ordering.eq
    | Timestamp.infinity, _ => Ordering.gt
    | _, Timestamp.infinity => Ordering.lt

instance : LT Timestamp where
  lt t1 t2 := match t1, t2 with
    | Timestamp.t time1, Timestamp.t time2 => time1 < time2
    | Timestamp.t _, Timestamp.infinity => true
    | Timestamp.infinity, _ => false

instance : Max Timestamp where
  max t1 t2 := match t1, t2 with
    | Timestamp.t time1, Timestamp.t time2 => if time1 >= time2 then t1 else t2
    | Timestamp.infinity, _ => t1
    | _, Timestamp.infinity => t2

instance : Min Timestamp where
  min t1 t2 := match t1, t2 with
    | Timestamp.t time1, Timestamp.t time2 => if time1 <= time2 then t1 else t2
    | Timestamp.infinity, _ => t2
    | _, Timestamp.infinity => t1

def Timestamp.sub (t1 t2 : Timestamp) : Timestamp :=
  match t1, t2 with
  | Timestamp.t time1, Timestamp.t time2 => if time1 > time2 then Timestamp.t (time1 - time2) else Timestamp.t 0
  | Timestamp.infinity, _ => Timestamp.infinity
  | _, Timestamp.infinity => Timestamp.t 0

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
  deriving BEq, Hashable, Repr, Ord, DecidableEq

instance : LT TimeDelta where
  lt t1 t2 := t1.dt < t2.dt

def TimeDelta.map (f : UInt64 -> UInt64) (td : TimeDelta) : TimeDelta :=
  { dt := f td.dt }

def Timestamp.add_delta (t1 : Timestamp) (t2 : TimeDelta) : Timestamp :=
  match t1 with
  | Timestamp.t time1 => Timestamp.t (time1 + t2.dt)
  | _ => Timestamp.infinity

def Timestamp.sub_delta (t1 : Timestamp) (t2 : TimeDelta) : Timestamp :=
  match t1 with
  | Timestamp.t time1 => Timestamp.t (time1 - t2.dt)
  | _ => Timestamp.infinity

def TimeDelta.toTimestamp (td : TimeDelta) : Timestamp :=
  Timestamp.t td.dt

def Timestamp.toTimeDelta (t : Timestamp) : TimeDelta :=
  match t with
  | Timestamp.t time => { dt := time }
  | _ => { dt := 0 }

theorem TimeDelta.inv (td : TimeDelta) : td = td.toTimestamp.toTimeDelta :=
  by unfold toTimestamp Timestamp.toTimeDelta <;> simp

namespace Interval
  def T : Type := (Timestamp × Timestamp)
  deriving  BEq, Hashable, Repr

  def T.map (f : Timestamp -> Timestamp) (i : T) : T :=
    (f i.1, f i.2)

  def T.incr_start (i : T) (d : TimeDelta) : T :=
    (Timestamp.add_delta i.1 d, i.2)

  def T.incr_end (i : T) (d : TimeDelta) : T :=
    (i.1, Timestamp.add_delta i.2 d)

  def T.incr (i : T) (d : TimeDelta) : T :=
    (Timestamp.add_delta i.1 d, Timestamp.add_delta i.2 d)

  def T.decr_start (i : T) (d : TimeDelta) : T :=
    (Timestamp.sub_delta i.1 d, i.2)

  def T.decr_end (i : T) (d : TimeDelta) : T :=
    (i.1, Timestamp.sub_delta i.2 d)

  def T.decr (i : T) (d : TimeDelta) : T :=
    (Timestamp.sub_delta i.1 d, Timestamp.sub_delta i.2 d)

  def T.contains (i : T) (t : Timestamp) : Bool :=
    i.1.before t && t.before i.2

  def T.intersection (i1 i2 : T) : Option T :=
    if i1.2.before i2.1 || i2.2.before i1.1 then
      none
    else
      some (max i1.1 i2.1, min i1.2 i2.2)

  -- todo: subinterval

end Interval

def Window : Type := Interval.T deriving BEq, Hashable, Repr
