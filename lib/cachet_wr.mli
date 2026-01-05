type 'fd t
type 'fd writev = 'fd -> pos:int -> Bstr.t list -> unit

val make :
     ?cachesize:int
  -> ?pagesize:int
  -> map:'fd Cachet.map
  -> writev:'fd writev
  -> 'fd
  -> 'fd t

val commit : 'fd t -> unit
val persist : 'fd t -> off:int -> len:int -> unit

val get_int8 : 'fd t -> int -> int
(** [get_int8 t logical_address] is [t]'s signed 8-bit integer starting at byte
    index [logical_address].

    @raise Out_of_bounds if [logical_address] is not accessible. *)

val get_uint8 : 'fd t -> int -> int
(** [get_uint8 t logical_address] is [t]'s unsigned 8-bit integer starting at
    byte index [logical_address].

    @raise Out_of_bounds if [logical_address] is not accessible. *)

val get_uint16_ne : 'fd t -> int -> int
(** [get_uint16_ne t i] is [t]'s native-endian unsigned 16-bit integer starting
    at byte index [i]. *)

val get_uint16_le : 'fd t -> int -> int
(** [get_uint16_le t i] is [t]'s little-endian unsigned 16-bit integer starting
    at byte index [i]. *)

val get_uint16_be : 'fd t -> int -> int
(** [get_uint16_be t i] is [t]'s big-endian unsigned 16-bit integer starting at
    byte index [i]. *)

val get_int16_ne : 'fd t -> int -> int
(** [get_int16_be t i] is [t]'s native-endian signed 16-bit integer starting at
    byte index [i]. *)

val get_int16_le : 'fd t -> int -> int
(** [get_int16_le t i] is [t]'s little-endian signed 16-bit integer starting at
    byte index [i]. *)

val get_int16_be : 'fd t -> int -> int
(** [get_int16_be t i] is [t]'s big-endian signed 16-bit integer starting at
    byte index [i]. *)

val get_int32_ne : 'fd t -> int -> int32
(** [get_int32_ne t i] is [t]'s native-endian 32-bit integer starting at byte
    index [i]. *)

val get_int32_le : 'fd t -> int -> int32
(** [get_int32_le t i] is [t]'s little-endian 32-bit integer starting at byte
    index [i]. *)

val get_int32_be : 'fd t -> int -> int32
(** [get_int32_be t i] is [t]'s big-endian 32-bit integer starting at byte index
    [i]. *)

val get_int64_ne : 'fd t -> int -> int64
(** [get_int64_ne t i] is [t]'s native-endian 64-bit integer starting at byte
    index [i]. *)

val get_int64_le : 'fd t -> int -> int64
(** [get_int64_le t i] is [t]'s little-endian 64-bit integer starting at byte
    index [i]. *)

val get_int64_be : 'fd t -> int -> int64
(** [get_int64_be t i] is [t]'s big-endian 64-bit integer starting at byte index
    [i]. *)

val get_int128 : 'fd t -> int -> string
val set_int8 : 'fd t -> int -> int -> unit
val set_uint8 : 'fd t -> int -> int -> unit
val set_uint16_ne : 'fd t -> int -> int -> unit
val set_uint16_le : 'fd t -> int -> int -> unit
val set_uint16_be : 'fd t -> int -> int -> unit
val set_int16_ne : 'fd t -> int -> int -> unit
val set_int16_le : 'fd t -> int -> int -> unit
val set_int16_be : 'fd t -> int -> int -> unit
val set_int32_ne : 'fd t -> int -> int32 -> unit
val set_int32_le : 'fd t -> int -> int32 -> unit
val set_int32_be : 'fd t -> int -> int32 -> unit
val set_int64_ne : 'fd t -> int -> int64 -> unit
val set_int64_le : 'fd t -> int -> int64 -> unit
val set_int64_be : 'fd t -> int -> int64 -> unit
val set_int128 : 'fd t -> int -> string -> unit

(**/*)

type sign = Unsigned | Signed
type endian = Le | Be | Ne

type 'a v =
  | Vi8 : sign -> int v
  | Vi16 : sign * endian -> int v
  | Vi32 : endian -> int32 v
  | Vi64 : endian -> int64 v
  | Vi128 : string v

type value = Value : 'a v * 'a -> value

val get : 'fd t -> int -> 'a v -> 'a
val set : 'fd t -> int -> 'a v -> 'a -> unit
