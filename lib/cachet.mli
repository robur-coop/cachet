type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val memcpy :
  bigstring -> src_off:int -> bigstring -> dst_off:int -> len:int -> unit

val memmove :
  bigstring -> src_off:int -> bigstring -> dst_off:int -> len:int -> unit

module Bstr : sig
  (** A read-only bigstring. *)

  type t = private bigstring

  val empty : t
  (** [empty] is an empty bigstring. *)

  val of_bigstring : bigstring -> t

  val length : t -> int
  (** [length bstr] is the number of bytes in [bstr]. *)

  val get : t -> int -> char
  (** [get bstr i] is the byte of [bstr]' at index [i]. This is equivalent to
      the [bstr.{i}] notation.

      @raise Invalid_argument if [i] is not an index of [bstr]. *)

  val get_int8 : t -> int -> int
  (** [get_int8 bstr i] is [bstr]'s signed 8-bit integer starting at byte index
      [i]. *)

  val get_uint8 : t -> int -> int
  (** [get_uint8 bstr i] is [bstr]'s unsigned 8-bit integer starting at byte
      index [i]. *)

  val get_uint16_ne : t -> int -> int
  (** [get_int16_ne bstr i] is [bstr]'s native-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_uint16_le : t -> int -> int
  (** [get_int16_le bstr i] is [bstr]'s little-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_uint16_be : t -> int -> int
  (** [get_int16_be bstr i] is [bstr]'s big-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_int16_ne : t -> int -> int
  (** [get_int16_ne bstr i] is [bstr]'s native-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int16_le : t -> int -> int
  (** [get_int16_le bstr i] is [bstr]'s little-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int16_be : t -> int -> int
  (** [get_int16_be bstr i] is [bstr]'s big-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int32_ne : t -> int -> int32
  (** [get_int32_ne bstr i] is [bstr]'s native-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int32_le : t -> int -> int32
  (** [get_int32_le bstr i] is [bstr]'s little-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int32_be : t -> int -> int32
  (** [get_int32_be bstr i] is [bstr]'s big-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int64_ne : t -> int -> int64
  (** [get_int64_ne bstr i] is [bstr]'s native-endian 64-bit integer starting at
      byte index [i]. *)

  val get_int64_le : t -> int -> int64
  (** [get_int64_le bstr i] is [bstr]'s little-endian 64-bit integer starting at
      byte index [i]. *)

  val get_int64_be : t -> int -> int64
  (** [get_int64_be bstr i] is [bstr]'s big-endian 64-bit integer starting at
      byte index [i]. *)

  val sub : t -> off:int -> len:int -> t
  (** [sub bstr ~off ~len] does not allocate a bigstring, but instead returns a
      new view into [bstr] starting at [off], and with length [len].

      {b Note} that this does not allocate a new buffer, but instead shares the
      buffer of [bstr] with the newly-returned bigstring. *)

  val sub_string : t -> off:int -> len:int -> string
  (** [sub_string bstr ~off ~len] returns a string of length [len] containing
      the bytes of [t] starting at [off]. *)

  val to_string : t -> string
  (** [to_string bstr] is equivalent to
      [sub_string bstr ~off:0 ~len:(length bstr)]. *)

  val blit_to_bytes :
    t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
  (** [blit_to_bytes src ~src_off dst ~dst_off ~len] copies [len] bytes from
      [src], starting at index [src_off], to byte sequence [dst], starting at
      index [dst_off].

      @raise Invalid_argument
        if [src_off] and [len] do not designate a valid range of [src], or if
        [dst_off] and [len] do not designate a valid range of [dst]. *)

  val is_empty : t -> bool
  (** [is_empty bstr] is [length bstr = 0]. *)

  val is_prefix : affix:string -> t -> bool
  (** [is_prefix ~affix bstr] is [true] iff [affix.[idx] = bstr.{idx}] for all
      indices [idx] of [affix]. *)

  val is_infix : affix:string -> t -> bool
  (** [is_infix ~affix bstr] is [true] iff there exists an index [j] in [bstr]
      such that for all indices [i] of [affix] we have
      [affix.[i] = bstr.{j + i}]. *)

  val is_suffix : affix:string -> t -> bool
  (** [is_suffix ~affix bstr] is [true] iff [affix.[n - idx] = bstr.{m - idx}]
      for all indices [idx] of [affix] with [n = String.length affix - 1] and
      [m = length bstr - 1]. *)

  val for_all : (char -> bool) -> t -> bool
  (** [for_all p bstr] is [true] iff for all indices [idx] of [bstr],
      [p bstr.{idx} = true]. *)

  val exists : (char -> bool) -> t -> bool
  (** [exists p bstr] is [true] iff there exists an index [idx] of [bstr] with
      [p bstr.{idx} = true]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [a = b]. *)

  val with_range : ?first:int -> ?len:int -> t -> t
  (** [with_range ~first ~len bstr] are the consecutive bytes of [bstr] whose
      indices exist in the range \[[first];[first + len - 1]\].

      [first] defaults to [0] and [len] to [max_int]. Note that [first] can be
      any integer and [len] any positive integer. *)

  val with_index_range : ?first:int -> ?last:int -> t -> t
  (** [with_index_range ~first ~last bstr] are the consecutive bytes of [bstr]
      whose indices exists in the range \[[first];[last]\].

      [first] defaults to [0] and [last] to [length bstr - 1].

      Note that both [first] and [last] can be any integer. If [first > last]
      the interval is empty and the empty bigstring is returned. *)

  val trim : ?drop:(char -> bool) -> t -> t
  (** [trim ~drop bstr] is [bstr] with prefix and suffix bytes satisfying [drop]
      in [bstr] removed. [drop] defaults to [fun chr -> chr = ' ']. *)

  val span :
    ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t
  (** [span ~rev ~min ~max ~sat bstr] is [(l, r)] where:
      - if [rev] is [false] (default), [l] is at least [min] and at most [max]
        consecutive [sat] satisfying initial bytes of [bstr] or {!empty} if
        there are no such bytes. [r] are the remaining bytes of [bstr].
      - if [rev] is [true], [r] is at least [min] and at most [max] consecutive
        [sat] satisfying final bytes of [bstr] or {!empty} if there are no such
        bytes. [l] are the remaining bytes of [bstr].

      If [max] is unspecified the span is unlimited. If [min] is unspecified it
      defaults to [0]. If [min > max] the condition can't be satisfied and the
      left or right span, depending on [rev], is always empty. [sat] defaults to
      [Fun.const true].

      @raise Invalid_argument if [max] or [min] is negative. *)

  val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
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
    [bigstring] must be returned if e.g. the position is out of range. Depending
    on how the cache is configured (see {!val:make}), [map] never read more than
    [pagesize] bytes. *)

(** {2 Note about schedulers and [Cachet].}

    [Cachet] assumes that {!type:map} is {b atomic}, in other words: {!type:map}
    is a unit of work that is indivisible and guaranteed to be executed as a
    single, coherent, and uninterrupted operation.

    In this way, the [map] function is considered as a "direct" computation that
    does {b not} interact with a scheduler. However, reading a page can take
    time. It may therefore be necessary to add a cooperation point after
    {!val:load} or the {{!user_friendly} user-friendly functions}.

    These functions can read one or more pages. {!val:load} reads one page at
    most.

    {2 Note about large file and [Cachet].}

    For performance reasons, Cachet has chosen to use an [int] rather than an
    [int64] for the offset (the logical address). On a 64-bit architecture,
    addressing in the block device should not be a problem and Cachet is able to
    manage large block devices. However, on a 32-bit architecture, Cachet should
    only be able to handle ~2 GB files.

    We consider that it is up to the developer to check this:
    {[
      let _max_int31 = 2147483647L (* (1 lsl 31) - 1 *)

      let () =
        let fd = Unix.openfile "disk.img" Unix.[ O_RDONLY ] 0o644 in
        let stat = Unix.LargeFile.fstat fd in
        if Sys.word_size = 32 && stat.Unix.LargeFile.st_size > _max_int31
        then failwith "Too big block-device";
        ...
    ]}

    So that, as soon as possible, the user can find out whether or not the
    program can handle large block-devices. *)

type 'fd t

val fd : 'fd t -> 'fd
val pagesize : 'fd t -> int

val cache_hit : 'fd t -> int
(** [cache_hit t] is the number of times a load hit the cache. *)

val cache_miss : 'fd t -> int
(** [cache_miss t] is the number of times a load didn't hit the cache. *)

val copy : 'fd t -> 'fd t
(** [copy t] creates a new, empty cache using the same [map] function. *)

val make : ?cachesize:int -> ?pagesize:int -> map:'fd map -> 'fd -> 'fd t
(** [make ~cachesize ~pagesize ~map fd] creates a new, empty cache using [map]
    and [fd] for reading [pagesize] bytes. The size of the cache is [cachesize].

    @raise Invalid_argument
      if either [cachesize] or [pagesize] is not a power of two. *)

val load : 'fd t -> ?len:int -> int -> slice option
(** [load t ~len logical_address] loads a page at the given [logical_address]
    and returns a {!type:slice}. [len] (defaults to [1]) is the expected minimum
    number of bytes returned.

    If the slice does not contains, at least, [len] bytes, [load] returns
    [None]. [load t ~len:0 logical_address] always returns an empty slice. *)

val invalidate : 'fd t -> off:int -> len:int -> unit
(** [invalidate t ~off ~len] invalidates the cache on [len] bytes from [off]. *)

val is_cached : 'fd t -> int -> bool
(** [is_cached t logical_address] returns [true] if the [logicial_address]
    requested is available in the cache, otherwise [false]. *)

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

exception Out_of_bounds of int
(** If Cachet tries to retrieve a byte outside the block device, this exception
    is raised. *)

val get_int8 : 'fd t -> int -> int
(** [get_int8 t logical_address] is [t]'s signed 8-bit integer starting at byte
    index [logical_address].

    @raise Out_of_bounds if [logical_address] is not accessible. *)

val get_uint8 : 'fd t -> int -> int
(** [get_uint8 t logical_address] is [t]'s unsigned 8-bit integer starting at
    byte index [logical_address].

    @raise Out_of_bounds if [logical_address] is not accessible. *)

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

    You can use {!val:syscalls} to find out how many times [get_string] can call
    [map] at most.

    @raise Out_of_bounds
      if [logical_address] and [len] byte(s) are not accessible. *)

val get_seq : 'fd t -> int -> string Seq.t
val next : 'fd t -> slice -> slice option
val iter : 'fd t -> ?len:int -> fn:(int -> unit) -> int -> unit

val blit_to_bytes :
  'fd t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
(** [blit_to_bytes t ~src_off dst ~dst_off ~len] copies [len] bytes from the
    cached {i block-device} represented by [t], starting at index [src_off] as
    the logical address, to byte sequence [dst], starting at index [dst_off].

    This function can read several pages depending on the size of the [dst]
    buffer.

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of the
      {i block-device}, or if [dst_off] and [len] do not designate a valid range
      of [dst]. *)

val syscalls : 'fd t -> logical_address:int -> len:int -> int
(** [syscalls t ~logicial_address ~len] returns the maximum number (if the cache
    is empty) of calls to [map] to load a segment of the block-device according
    to the [logical_address] and the size [len] (in bytes) of the segment. *)
