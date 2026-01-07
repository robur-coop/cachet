(** [Cachet_wr] is a module that allows writing to a memory area that is
    accessed via pages and to which you can write via pages. In order to
    optimise writing, [Cachet_wr] offers a {i write pipeline} that is only
    effective via {!val:commit} or {!val:persist}: i.e. the user can write data,
    but it will be delayed in a pipeline. These writes will only be applied to
    the memory area after a {!val:commit} (which flushes our entire write
    pipeline) or a {!val:persist} (to flush only part of our pipeline). Reading
    via [Cachet_wr] takes this pipeline into account, i.e. after a write, it
    appears to be effective from the user's point of view but is not actually
    physically effective.

    The idea is that write operations do not have a real write cost
    ({!type:writev}) and can be {i cached} until the user wants them to take
    effect in the memory area via {!val:persist}/{!val:commit}.

    [Cachet_wr] ensures consistency between writes and reads, i.e. a read takes
    into account the writes in the cache in our pipeline but also what is
    actually in memory. [Cachet_wr] is based on {!module:Cachet} in order to
    also limit calls to {!type:Cachet.map}.

    [Cachet_wr] only handles so-called {i atomic} values, which have a fixed
    size. The meaning of atomic does not refer to parallelism ([Cachet] and
    [Cachet_wr] are {b not} domain-safe), but rather to the fact that there are
    no interruptions (and no opportunity for other tasks to run) when writing.
    Only {!val:commit}, {!val:persist} and reading (via {!type:Cachet.map}) can
    reorder the execution of tasks (depending on their implementations). *)

type 'fd t
(** Type of cachet.wr's values. *)

type 'fd writev = 'fd -> pos:int -> Bstr.t list -> unit
(** A [writev : 'fd writev] value, when applied [writev fd ~pos bstrs], writes
    the given continuous pages [bstrs] to a specific aligned offset [pos]. *)

val make :
     ?cachesize:int
  -> ?pagesize:int
  -> map:'fd Cachet.map
  -> writev:'fd writev
  -> number_of_pages:int
  -> 'fd
  -> 'fd t

val commit : 'fd t -> unit
(** [commit t] flushes the write pipeline completely and effectively apply all
    writes by issuing the latest version of the pages via one or more calls to
    {!type:writev}. *)

val persist : 'fd t -> off:int -> len:int -> unit
(** [persist t ~off ~len] flushes part of the write pipeline that concerns the
    area requested by [\[off..len\[]. [persist] should issue, at most, a single
    call to {!type:writev} in order to update the relevant continuous pages. It
    is possible that [persist] may decide to make certain writes effective that
    are not in the requested area [\[off..len\[] because they overlap the
    requested area. *)

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
(** [get_int128 t off] is [t]'s 128-bit integer starting at byte index [off]. *)

val set_int8 : 'fd t -> int -> int -> unit
(** [set_int8 t off v] sets [t]'s signed 8-bit integer starting at byte index
    [off] to [v]. *)

val set_uint8 : 'fd t -> int -> int -> unit
(** [set_uint8 t off v] sets [t]'s unsigned 8-bit integer starting at byte index
    [off] to [v]. *)

val set_uint16_ne : 'fd t -> int -> int -> unit
(** [set_uint16_ne t off v] sets [t]'s native-endian unsigned 16-bit integer
    starting at byte index [off] to [v]. *)

val set_uint16_le : 'fd t -> int -> int -> unit
(** [set_uint16_le t off v] sets [t]'s little-endian unsigned 16-bit integer
    starting at byte index [off] to [v]. *)

val set_uint16_be : 'fd t -> int -> int -> unit
(** [set_uint16_be t off v] sets [t]'s big-endian unsigned 16-bit integer
    starting at byte index [off] to [v]. *)

val set_int16_ne : 'fd t -> int -> int -> unit
(** [set_int16_ne t off v] sets [t]'s native-endian signed 16-bit integer
    starting at byte index [off] to [v]. *)

val set_int16_le : 'fd t -> int -> int -> unit
(** [set_int16_le t off v] sets [t]'s little-endian signed 16-bit integer
    starting at byte index [off] to [v]. *)

val set_int16_be : 'fd t -> int -> int -> unit
(** [set_int16_be t off v] sets [t]'s big-endian signed 16-bit integer starting
    at byte index [off] to [v]. *)

val set_int32_ne : 'fd t -> int -> int32 -> unit
(** [set_int32_ne t off v] sets [t]'s native-endian 32-bit integer starting at
    byte index [off] to [v]. *)

val set_int32_le : 'fd t -> int -> int32 -> unit
(** [set_int32_le t off v] sets [t]'s little-endian 32-bit integer starting at
    byte index [off] to [v]. *)

val set_int32_be : 'fd t -> int -> int32 -> unit
(** [set_int32_be t off v] sets [t]'s big-endian 32-bit integer starting at byte
    index [off] to [v]. *)

val set_int64_ne : 'fd t -> int -> int64 -> unit
(** [set_int64_ne t off v] sets [t]'s native-endian 64-bit integer starting at
    byte index [off] to [v]. *)

val set_int64_le : 'fd t -> int -> int64 -> unit
(** [set_int64_le t off v] sets [t]'s little-endian 64-bit integer starting at
    byte index [off] to [v]. *)

val set_int64_be : 'fd t -> int -> int64 -> unit
(** [set_int64_be t off v] sets [t]'s big-endian 64-bit integer starting at byte
    index [off] to [v]. *)

val set_int128 : 'fd t -> int -> string -> unit
(** [set_int128 t off v] sets [t]'s 128-bit integer starting at byte index [off]
    to [v]. *)

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
