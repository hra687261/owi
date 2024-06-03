type !'a t

val init : unit -> 'a t

val push : 'a -> 'a t -> unit

val pop : 'a t -> bool -> 'a option

val make_pledge : 'a t -> unit

val end_pledge : 'a t -> unit

val fail : 'a t -> unit

val read_as_seq : 'a t -> finalizer:(unit -> unit) -> 'a Seq.t
