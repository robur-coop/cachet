type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Bstr : sig
  (** A read-only bigstring. *)

  type t = private bigstring

  val empty : t
  (** [empty] is an empty bigstring. *)

  val of_bigstring : bigstring -> t

  val length : t -> int
  (** [length bstr] is the number of bytes in [bstr]. *)

  val get : t -> int -> char
  (** [get bstr i] is the byte of [bstr]' at index [i]. This is
      equivalent to the [bstr.{i}] notation. 

      @raise Invalid_argument if [i] is not an index of [bstr]. *)

  val get_int8 : t -> int -> int
  val get_uint8 : t -> int -> int
  val get_int16_ne : t -> int -> int
  val get_int16_le : t -> int -> int
  val get_int16_be : t -> int -> int
  val get_int32_ne : t -> int -> int32
  val get_int32_le : t -> int -> int32
  val get_int32_be : t -> int -> int32
  val get_int64_ne : t -> int -> int64
  val get_int64_le : t -> int -> int64
  val get_int64_be : t -> int -> int64

  val sub : t -> off:int -> len:int -> t
  (** [sub bstr ~off ~len] does not allocate a bigstring, but instead returns a new
      view into [bstr] starting at [off], and with length [len].

      {b Note} that this does not allocate a new buffer, but instead shares the
      buffer of [bstr] with the newly-returned bigstring. *)

  val sub_string : t -> off:int -> len:int -> string
  (** [sub_string bstr ~off ~len] returns a string of length [len] containing the
      bytes of [t] starting at [off]. *)

  val to_string : t -> string
  (** [to_string bstr] is equivalent to [sub_string bstr ~off:0 ~len:(length bstr)]. *)

  val blit_to_bytes :
    t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
  (** [blit_to_bytes src ~src_off dst ~dst_off ~len] copies [len] bytes from
      [src], starting at index [src_off], to byte sequence [dst], starting at
      index [dst_off].

      @raise Invalid_argument if [src_off] and [len] do not designate a valid
      range of [src], or if [dst_off] and [len] do not designate a valid range
      of [dst]. *)

  val is_empty : t -> bool

  (*
  val is_prefix : affix:string -> t -> bool
  val is_infix : affix:string -> t -> bool
  val is_suffix : affix:string -> t -> bool
  val for_all : (char -> bool) -> t -> bool
  val exists : (char -> bool) -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val with_range : ?first:int -> ?len:int -> t -> t
  val with_index_range : ?first:int -> ?last:int -> t -> t
  val trim : ?drop:(char -> bool) -> t -> t
  val span : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t
  val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val cut : ?rev:bool -> sep:string -> t -> (t * t) option
  val cuts : ?rev:bool -> ?empty:bool -> sep:string -> t -> t list
  *)
end

type slice = private { offset: int; length: int; payload: Bstr.t }
(** A slice is an aligned segment of bytes (according to the [pagesize]
    specified by the cache, see {!val:make}) with its absolute position into the
    underlying {i block-device} and size. *)

val pp_slice : Format.formatter -> slice -> unit
val bstr_of_slice : ?logical_address:int -> slice -> Bstr.t

type 'fd map = 'fd -> pos:int -> int -> bigstring
(** A value [map : 'fd map] when applied [map fd ~pos len] reads a
    {!type:bigstring} at [pos]. [map] must return as much data as is available,
    though never more than [len] bytes. [map] never fails. Instead, an empty
    [bigstring] must be returned if e.g. the position is out of range.
    Depending on how the cache is configured (see {!val:make}), [map] never
    read more than [pagesize] bytes. *)

(** {2 Note about schedulers and [Cachet].}

    [Cachet] assumes that {!type:map} is {b atomic}, in other words: {!type:map}
    is a unit of work that is indivisible and guaranteed to be executed as a
    single, coherent, and uninterrupted operation.

    In this way, the [map] function is considered as a "direct" computation that
    does {b not} interact with a scheduler. However, reading a page can take
    time. It may therefore be necessary to add a cooperation point after
    {!val:load} or the {{!user_friendly} user-friendly functions}.

    These functions can read one or more pages. {!val:load} reads one page at
    most. *)

type 'fd t

val fd : 'fd t -> 'fd

val cache_hit : 'fd t -> int
(** [cache_hit t] is the number of times a load hit the cache. *)

val cache_miss : 'fd t -> int
(** [cache_miss t] is the number of times a load didn't hit the cache. *)

val copy : 'fd t -> 'fd t
(** [copy t] creates a new, empty cache using the same [map] function. *)

val make : ?cachesize:int -> ?pagesize:int -> map:'fd map -> 'fd -> 'fd t
(** [make ~cachesize ~pagesize ~map fd] creates a new, empty cache using [map]
    and [fd] for reading [pagesize] bytes. The size of the cache is [cachesize].

    @raise Invalid_argument if either [cachesize] or [pagesize] is not a power
    of two. *)

val load : 'fd t -> ?len:int -> int -> slice option
(** [load t ~len logical_address] loads a page at the given [logical_address]
    and returns a {!type:slice}. [len] (defaults to [1]) is the expected
    minimum number of bytes returned.

    If the slice does not contains, at least, [len] bytes, [load] returns [None].
    [load t ~len:0 logical_address] always returns an empty slice. *)

val invalidate : 'fd t -> off:int -> len:int -> unit
(** [invalidate t ~off ~len] invalidates the cache on [len] bytes from [off]. *)

(** {2:user_friendly User friendly functions.} *)

(** {3 Binary decoding of integers.}

    The functions in this section binary decode integers from byte sequences.

    All following functions raise [Invalid_argument] if the space needed at
    index [i] to decode the integer is not available.

    Little-endian (resp. big-endian) encoding means that least (resp. most)
    significant bytes are stored first. Big-endian is also known as network byte
    order. Native-endian encoding is either little-endian or big-endian
    depending on {!Sys.big_endian}.

    32-bit and 64-bit integers are represented by the [int] type, which has more
    bits than the binary encoding. Functions that decode signed (resp. unsigned)
    8-bit or 16-bit integers represented by [int] values sign-extend (resp.
    zero-extend) their result. *)

val get_int8 : 'fd t -> int -> int
(** [get_int8 t logical_address] is [t]'s signed 8-bit integer starting at byte
    index [logical_address]. *)

val get_uint8 : 'fd t -> int -> int
(** [get_uint8 t logical_address] is [t]'s unsigned 8-bit integer starting at byte
    index [logical_address]. *)

val get_uint16_ne : 'fd t -> int -> int
val get_uint16_le : 'fd t -> int -> int
val get_uint16_be : 'fd t -> int -> int
val get_int16_ne : 'fd t -> int -> int
val get_int16_le : 'fd t -> int -> int
val get_int16_be : 'fd t -> int -> int
val get_int32_ne : 'fd t -> int -> int32
val get_int32_le : 'fd t -> int -> int32
val get_int32_be : 'fd t -> int -> int32
val get_int64_ne : 'fd t -> int -> int64
val get_int64_le : 'fd t -> int -> int64
val get_int64_be : 'fd t -> int -> int64

val get_string : 'fd t -> len:int -> int -> string
(** [get_string t ~len logical_address] loads the various pages needed from the
    cache or using [map] to copy [len] bytes available at [off].

    You can use {!val:syscalls} to find out how many times [get_string] can
    call [map] at most.

    @raise Failure if the [map] function cannot give us enough to copy [len]
    bytes. *)

val get_seq : 'fd t -> int -> string Seq.t
val next : 'fd t -> slice -> slice option
val iter : 'fd t -> ?len:int -> fn:(int -> unit) -> int -> unit

val blit_to_bytes :
  'fd t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
(** [blit_to_bytes t ~src_off dst ~dst_off ~len] copies [len] bytes from
    the cached {i block-device} represented by [t], starting at index [src_off]
    as the logical address, to byte sequence [dst], starting at index
    [dst_off].

    This function can read several pages depending on the size of the [dst]
    buffer.

    @raise Invalid_argument if [src_off] and [len] do not designate a valid
    range of the {i block-device}, or if [dst_off] and [len] do not designate a
    valid range of [dst]. *)

val syscalls : 'fd t -> logical_address:int -> len:int -> int
(** [syscalls t ~logicial_address ~len] returns the maximum number (if the cache
    is empty) of calls to [map] to load a segment of the block-device according
    to the [logical_address] and the size [len] (in bytes) of the segment. *)
