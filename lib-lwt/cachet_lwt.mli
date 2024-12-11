(** The Lwt variation of Cachet implies a point of cooperation ([Lwt.pause]) as
    soon as the syscall [map] is called. In other words, a task developed with
    Cachet_lwt will make itself available to be rescheduled if we internally
    call [map] instead of using the cache.

    In the event that the functions below were to use the cache, they would
    retain the exclusive right to execute and would not allow any cooperation
    points to appear.

    Such an approach increases the task's availability if it does I/O in
    cooperation with other tasks that would also like to do I/O. *)

val load : 'fd Cachet.t -> ?len:int -> int -> Cachet.slice option Lwt.t

val get_int8 : 'fd Cachet.t -> int -> int Lwt.t
(** [get_int8 t logical_address] is [t]'s signed 8-bit integer starting at byte
    index [logical_address].

    @raise Out_of_bounds if [logical_address] is not accessible. *)

val get_uint8 : 'fd Cachet.t -> int -> int Lwt.t
(** [get_uint8 t logical_address] is [t]'s unsigned 8-bit integer starting at
    byte index [logical_address].

    @raise Out_of_bounds if [logical_address] is not accessible. *)

val get_uint16_ne : 'fd Cachet.t -> int -> int Lwt.t
val get_uint16_le : 'fd Cachet.t -> int -> int Lwt.t
val get_uint16_be : 'fd Cachet.t -> int -> int Lwt.t
val get_int16_ne : 'fd Cachet.t -> int -> int Lwt.t
val get_int16_le : 'fd Cachet.t -> int -> int Lwt.t
val get_int16_be : 'fd Cachet.t -> int -> int Lwt.t
val get_int32_ne : 'fd Cachet.t -> int -> int32 Lwt.t
val get_int32_le : 'fd Cachet.t -> int -> int32 Lwt.t
val get_int32_be : 'fd Cachet.t -> int -> int32 Lwt.t
val get_int64_ne : 'fd Cachet.t -> int -> int64 Lwt.t
val get_int64_le : 'fd Cachet.t -> int -> int64 Lwt.t
val get_int64_be : 'fd Cachet.t -> int -> int64 Lwt.t
val get_string : 'fd Cachet.t -> len:int -> int -> string Lwt.t
val next : 'fd Cachet.t -> Cachet.slice -> Cachet.slice option Lwt.t

val blit_to_bytes :
  'fd Cachet.t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit Lwt.t
