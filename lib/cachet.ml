type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external get_int32_ne : bigstring -> int -> int32 = "%caml_bigstring_get32"

external set_int32_ne : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

let memcpy src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "memcpy";
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = get_int32_ne src (src_off + i) in
    set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = get_uint8 src (src_off + i) in
    set_uint8 dst (dst_off + i) v
  done

let memmove src ~src_off dst ~dst_off ~len =
  let src = Bigarray.Array1.sub src src_off len in
  let dst = Bigarray.Array1.sub dst dst_off len in
  Bigarray.Array1.blit src dst

let invalid_argf fmt = Format.kasprintf invalid_arg fmt

module Bstr = struct
  type t = bigstring

  let of_bigstring x = x
  let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
  let length = Bigarray.Array1.dim

  external get : t -> int -> char = "%caml_ba_ref_1"
  external get_uint8 : t -> int -> int = "%caml_ba_ref_1"
  external get_uint16_ne : t -> int -> int = "%caml_bigstring_get16"
  external get_int32_ne : t -> int -> int32 = "%caml_bigstring_get32"
  external get_int64_ne : t -> int -> int64 = "%caml_bigstring_get64"

  let get_int8 bstr i =
    (get_uint8 bstr i lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

  let get_uint16_le bstr i =
    if Sys.big_endian then swap16 (get_uint16_ne bstr i)
    else get_uint16_ne bstr i

  let get_uint16_be bstr i =
    if not Sys.big_endian then swap16 (get_uint16_ne bstr i)
    else get_uint16_ne bstr i

  let get_int16_ne bstr i =
    (get_uint16_ne bstr i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let get_int16_le bstr i =
    (get_uint16_le bstr i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let get_int16_be bstr i =
    (get_uint16_be bstr i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let get_int32_le bstr i =
    if Sys.big_endian then swap32 (get_int32_ne bstr i) else get_int32_ne bstr i

  let get_int32_be bstr i =
    if not Sys.big_endian then swap32 (get_int32_ne bstr i)
    else get_int32_ne bstr i

  let get_int64_le bstr i =
    if Sys.big_endian then swap64 (get_int64_ne bstr i) else get_int64_ne bstr i

  let get_int64_be bstr i =
    if not Sys.big_endian then swap64 (get_int64_ne bstr i)
    else get_int64_ne bstr i

  let sub t ~off ~len = Bigarray.Array1.sub t off len

  let blit_to_bytes bstr ~src_off dst ~dst_off ~len =
    if
      len < 0
      || src_off < 0
      || src_off > length bstr - len
      || dst_off < 0
      || dst_off > Bytes.length dst - len
    then invalid_arg "Cachet.Bstr.blit_to_bytes";
    let len0 = len land 3 in
    let len1 = len lsr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = get_int32_ne bstr (src_off + i) in
      Bytes.set_int32_ne dst (dst_off + i) v
    done;
    for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = get_uint8 bstr (src_off + i) in
      Bytes.set_uint8 dst (dst_off + i) v
    done

  let sub_string bstr ~off ~len =
    let buf = Bytes.create len in
    blit_to_bytes bstr ~src_off:off buf ~dst_off:0 ~len;
    Bytes.unsafe_to_string buf

  let to_string bstr = sub_string bstr ~off:0 ~len:(length bstr)
  let is_empty bstr = length bstr == 0

  let is_prefix ~affix bstr =
    let len_affix = String.length affix in
    let len_bstr = length bstr in
    if len_affix > len_bstr then false
    else
      let max_idx_affix = len_affix - 1 in
      let rec go idx =
        if idx > max_idx_affix then true
        else if affix.[idx] != bstr.{idx} then false
        else go (succ idx)
      in
      go 0

  let is_infix ~affix bstr =
    let len_affix = String.length affix in
    let len_bstr = length bstr in
    if len_affix > len_bstr then false
    else
      let max_idx_affix = len_affix - 1 in
      let max_idx_bstr = len_bstr - len_affix in
      let rec go idx k =
        if idx > max_idx_bstr then false
        else if k > max_idx_affix then true
        else if k > 0 then
          if affix.[k] == bstr.{idx + k} then go idx (succ k)
          else go (succ idx) 0
        else if affix.[0] = bstr.{idx} then go idx 1
        else go (succ idx) 0
      in
      go 0 0

  let is_suffix ~affix bstr =
    let max_idx_affix = String.length affix - 1 in
    let max_idx_bstr = length bstr - 1 in
    if max_idx_affix > max_idx_bstr then false
    else
      let rec go idx =
        if idx > max_idx_affix then true
        else if affix.[max_idx_affix - idx] != bstr.{max_idx_bstr - idx} then
          false
        else go (succ idx)
      in
      go 0

  exception Break

  let for_all sat bstr =
    try
      for idx = 0 to length bstr - 1 do
        if sat bstr.{idx} == false then raise_notrace Break
      done;
      true
    with Break -> false

  let exists sat bstr =
    try
      for idx = 0 to length bstr - 1 do
        if sat bstr.{idx} then raise_notrace Break
      done;
      false
    with Break -> true

  let equal a b =
    if length a == length b then
      try
        let len = length a in
        let len0 = len land 3 in
        let len1 = len lsr 2 in
        for i = 0 to len1 - 1 do
          let i = i * 4 in
          if get_int32_ne a i <> get_int32_ne b i then raise_notrace Break
        done;
        for i = 0 to len0 - 1 do
          let i = (len1 * 4) + i in
          if get_uint8 a i != get_uint8 b i then raise_notrace Break
        done;
        true
      with Break -> false
    else false

  let with_range ?(first = 0) ?(len = max_int) bstr =
    if len < 0 then invalid_arg "Cachet.Bstr.with_range";
    if len == 0 then empty
    else
      let bstr_len = length bstr in
      let max_idx = bstr_len - 1 in
      let last =
        match len with
        | len when len = max_int -> max_idx
        | len ->
            let last = first + len - 1 in
            if last > max_idx then max_idx else last
      in
      let first = if first < 0 then 0 else first in
      if first = 0 && last = max_idx then bstr
      else sub bstr ~off:first ~len:(last + 1 - first)

  let with_index_range ?(first = 0) ?last bstr =
    let bstr_len = length bstr in
    let max_idx = bstr_len - 1 in
    let last =
      match last with
      | None -> max_idx
      | Some last -> if last > max_idx then max_idx else last
    in
    let first = if first < 0 then 0 else first in
    if first > max_idx || last < 0 || first > last then empty
    else if first == 0 && last = max_idx then bstr
    else sub bstr ~off:first ~len:(last + 1 - first)

  let is_white chr = chr == ' '

  let trim ?(drop = is_white) bstr =
    let len = length bstr in
    if len == 0 then bstr
    else
      let max_idx = len - 1 in
      let rec left_pos idx =
        if idx > max_idx then len
        else if drop bstr.{idx} then left_pos (succ idx)
        else idx
      in
      let rec right_pos idx =
        if idx < 0 then 0
        else if drop bstr.{idx} then right_pos (pred idx)
        else succ idx
      in
      let left = left_pos 0 in
      if left = len then empty
      else
        let right = right_pos max_idx in
        if left == 0 && right == len then bstr
        else sub bstr ~off:left ~len:(right - left)

  let fspan ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
    if min < 0 then invalid_arg "Cachet.Bstr.fspan";
    if max < 0 then invalid_arg "Cachet.Bstr.fspan";
    if min > max || max == 0 then (empty, bstr)
    else
      let len = length bstr in
      let max_idx = len - 1 in
      let max_idx =
        let k = max - 1 in
        if k > max_idx then max_idx else k
      in
      let need_idx = min in
      let rec go idx =
        if idx <= max_idx && sat bstr.{idx} then go (succ idx)
        else if idx < need_idx || idx == 0 then (empty, bstr)
        else if idx == len then (bstr, empty)
        else (sub bstr ~off:0 ~len:idx, sub bstr ~off:idx ~len:(len - idx))
      in
      go 0

  let rspan ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
    if min < 0 then invalid_arg "Cachet.Bstr.rspan";
    if max < 0 then invalid_arg "Cachet.Bstr.rspan";
    if min > max || max == 0 then (bstr, empty)
    else
      let len = length bstr in
      let max_idx = len - 1 in
      let min_idx =
        let k = len - max in
        if k < 0 then 0 else k
      in
      let need_idx = max_idx - min in
      let rec go idx =
        if idx >= min_idx && sat bstr.{idx} then go (pred idx)
        else if idx > need_idx || idx == max_idx then (bstr, empty)
        else if idx == -1 then (empty, bstr)
        else
          let cut = idx + 1 in
          (sub bstr ~off:0 ~len:cut, sub bstr ~off:cut ~len:(len - cut))
      in
      go 0

  let span ?(rev = false) ?min ?max ?sat bstr =
    match rev with
    | true -> rspan ?min ?max ?sat bstr
    | false -> fspan ?min ?max ?sat bstr

  let ftake ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
    if min < 0 then invalid_arg "Cachet.Bstr.ftake";
    if max < 0 then invalid_arg "Cachet.Bstr.ftake";
    if min > max || max == 0 then empty
    else
      let len = length bstr in
      let max_idx = len - 1 in
      let max_idx =
        let k = max - 1 in
        if k > max_idx then max_idx else k
      in
      let need_idx = min in
      let rec go idx =
        if idx <= max_idx && sat bstr.{idx} then go (succ idx)
        else if idx < need_idx || idx == 0 then empty
        else if idx == len then bstr
        else sub bstr ~off:0 ~len:idx
      in
      go 0

  let rtake ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
    if min < 0 then invalid_arg "Cachet.Bstr.rtake";
    if max < 0 then invalid_arg "Cachet.Bstr.rtake";
    if min > max || max == 0 then empty
    else
      let len = length bstr in
      let max_idx = len - 1 in
      let min_idx =
        let k = len - max in
        if k < 0 then 0 else k
      in
      let need_idx = max_idx - min in
      let rec go idx =
        if idx >= min_idx && sat bstr.{idx} then go (pred idx)
        else if idx > need_idx || idx == max_idx then empty
        else if idx == -1 then bstr
        else
          let cut = idx + 1 in
          sub bstr ~off:cut ~len:(len - cut)
      in
      go 0

  let take ?(rev = false) ?min ?max ?sat bstr =
    match rev with
    | true -> rtake ?min ?max ?sat bstr
    | false -> ftake ?min ?max ?sat bstr

  let fdrop ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
    if min < 0 then invalid_arg "Cachet.Bstr.fspan";
    if max < 0 then invalid_arg "Cachet.Bstr.fspan";
    if min > max || max == 0 then bstr
    else
      let len = length bstr in
      let max_idx = len - 1 in
      let max_idx =
        let k = max - 1 in
        if k > max_idx then max_idx else k
      in
      let need_idx = min in
      let rec go idx =
        if idx <= max_idx && sat bstr.{idx} then go (succ idx)
        else if idx < need_idx || idx == 0 then bstr
        else if idx == len then bstr
        else sub bstr ~off:idx ~len:(len - idx)
      in
      go 0

  let rdrop ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
    if min < 0 then invalid_arg "Cachet.Bstr.rspan";
    if max < 0 then invalid_arg "Cachet.Bstr.rspan";
    if min > max || max == 0 then bstr
    else
      let len = length bstr in
      let max_idx = len - 1 in
      let min_idx =
        let k = len - max in
        if k < 0 then 0 else k
      in
      let need_idx = max_idx - min in
      let rec go idx =
        if idx >= min_idx && sat bstr.{idx} then go (pred idx)
        else if idx > need_idx || idx == max_idx then bstr
        else if idx == -1 then empty
        else
          let cut = idx + 1 in
          sub bstr ~off:0 ~len:cut
      in
      go 0

  let drop ?(rev = false) ?min ?max ?sat bstr =
    match rev with
    | true -> rdrop ?min ?max ?sat bstr
    | false -> fdrop ?min ?max ?sat bstr

  let shift bstr off =
    if off > length bstr then invalid_arg "Cachet.Bstr.shift";
    let len = length bstr - off in
    Bigarray.Array1.sub bstr off len
end

external hash : (int32[@unboxed]) -> int -> (int32[@unboxed])
  = "cachet_hash_mix_intnat" "caml_hash_mix_intnat"
[@@noalloc]

let hash h d = Int32.to_int (hash h d)

type slice = { offset: int; length: int; payload: bigstring }

let pp_slice ppf { offset; length; _ } =
  Format.fprintf ppf "{ @[<hov>offset= %x;@ length= %d;@] }" offset length

(* Counter Trailing Zero *)
let unsafe_ctz n =
  let t = ref 1 in
  let r = ref 0 in
  while n land !t = 0 do
    t := !t lsl 1;
    incr r
  done;
  !r

let bstr_of_slice ?(logical_address = 0) { offset; length; payload } =
  if logical_address < 0 then invalid_arg "Cachet.bstr_of_slice";
  if logical_address == 0 || logical_address == offset then payload
  else if logical_address > offset + length then
    invalid_arg "Cachet.bstr_of_slice"
  else
    let pagesize = unsafe_ctz offset in
    let off = logical_address land ((pagesize lsl 1) - 1) in
    let len = length - off in
    Bstr.sub payload ~off ~len

type metrics = { mutable cache_hit: int; mutable cache_miss: int }

let metrics () = { cache_hit= 0; cache_miss= 0 }

type 'fd t = {
    arr: slice option array
  ; fd: 'fd
  ; map: 'fd map
  ; pagesize: int
  ; cachesize: int
  ; metrics: metrics
}

and 'fd map = 'fd -> pos:int -> int -> bigstring

let fd { fd; _ } = fd
let pagesize { pagesize; _ } = 1 lsl pagesize

let copy t =
  {
    arr= Array.make (1 lsl t.cachesize) None
  ; fd= t.fd
  ; map= t.map
  ; pagesize= t.pagesize
  ; cachesize= t.cachesize
  ; metrics= metrics ()
  }

(* XXX(dinosaure): power of two. *)
let pot x = x land (x - 1) == 0 && x != 0

let make ?(cachesize = 1 lsl 10) ?(pagesize = 1 lsl 12) ~map fd =
  if pot cachesize = false || pot pagesize = false then
    invalid_arg "Chat.make: cachesize or pagesize must be a power of two";
  let arr = Array.make cachesize None in
  let pagesize = unsafe_ctz pagesize in
  let cachesize = unsafe_ctz cachesize in
  let metrics = metrics () in
  { arr; fd; map; pagesize; cachesize; metrics }

let load t logical_address =
  let page = logical_address lsr t.pagesize in
  let payload = t.map t.fd ~pos:(page lsl t.pagesize) (1 lsl t.pagesize) in
  let length = Bigarray.Array1.dim payload in
  let slice = { offset= page lsl t.pagesize; length; payload } in
  let hash = hash 0l slice.offset land ((1 lsl t.cachesize) - 1) in
  t.arr.(hash) <- Some slice;
  slice

let none : slice option = None
let cache_miss t = t.metrics.cache_miss
let cache_hit t = t.metrics.cache_hit

let map ({ fd; map; _ } as t) ~pos:logical_address logical_len =
  let page = logical_address lsr t.pagesize in
  let pos = page lsl t.pagesize in
  (* round-down *)
  let rem = logical_address - pos in
  let len = rem + logical_len in
  let len =
    (* round-up *)
    if ((1 lsl t.pagesize) - 1) land len != 0 then
      (len + (1 lsl t.pagesize)) land lnot ((1 lsl t.pagesize) - 1)
    else len
  in
  let off = logical_address land ((1 lsl t.pagesize) - 1) in
  if len <= 1 lsl t.pagesize then begin
    let hash = hash 0l (page lsl t.pagesize) land ((1 lsl t.cachesize) - 1) in
    match t.arr.(hash) with
    | Some { offset; length; payload } when offset == page lsl t.pagesize ->
        t.metrics.cache_hit <- t.metrics.cache_hit + 1;
        let len = Int.min (length - off) logical_len in
        Bigarray.Array1.sub payload off len
    | Some _ | None ->
        t.metrics.cache_miss <- t.metrics.cache_miss + 1;
        let { length; payload; _ } = load t logical_address in
        let len = Int.min (length - off) logical_len in
        Bigarray.Array1.sub payload off len
  end
  else begin
    t.metrics.cache_miss <- t.metrics.cache_miss + 1;
    let bstr = map fd ~pos len in
    let len = Int.min (Bigarray.Array1.dim bstr - off) logical_len in
    Bigarray.Array1.sub bstr off len
  end

let load t ?(len = 1) logical_address =
  if len > 1 lsl t.pagesize then
    invalid_arg "Cachet.load: you can not load more than a page";
  if logical_address < 0 then
    invalid_argf "Cachet.load: a logical address must be positive (%08x)"
      logical_address;
  let page = logical_address lsr t.pagesize in
  let hash = hash 0l (page lsl t.pagesize) land ((1 lsl t.cachesize) - 1) in
  let offset = logical_address land ((t.pagesize lsl 1) - 1) in
  match t.arr.(hash) with
  | Some slice as value when slice.offset == page lsl t.pagesize ->
      t.metrics.cache_hit <- t.metrics.cache_hit + 1;
      if slice.length - offset >= len then value else none
  | Some _ | None ->
      t.metrics.cache_miss <- t.metrics.cache_miss + 1;
      let slice = load t logical_address in
      if slice.length - offset >= len then Some slice else none

let is_cached t logical_address =
  let page = logical_address lsr t.pagesize in
  let hash = hash 0l (page lsl t.pagesize) land ((1 lsl t.cachesize) - 1) in
  match t.arr.(hash) with
  | Some slice -> slice.offset == page lsl t.pagesize
  | None -> false

let invalidate t ~off:logical_address ~len =
  if logical_address < 0 || len < 0 then
    invalid_arg
      "Cachet.invalidate: the logical address and/or the number of bytes to \
       invalid must be positives";
  let start_page = logical_address lsr t.pagesize in
  let end_page = (logical_address + len) lsr t.pagesize in
  let mask = (1 lsl t.cachesize) - 1 in
  for i = start_page to end_page - 1 do
    t.arr.(hash 0l (i lsl t.pagesize) land mask) <- None
  done

let is_aligned x = x land ((1 lsl 2) - 1) == 0

exception Out_of_bounds of int

let[@inline never] out_of_bounds offset = raise (Out_of_bounds offset)

let get_uint8 t logical_address =
  match load t ~len:1 logical_address with
  | Some { payload; _ } ->
      let offset = logical_address land ((1 lsl t.pagesize) - 1) in
      Bstr.get_uint8 payload offset
  | None -> out_of_bounds logical_address

let get_int8 t logical_address =
  (get_uint8 t logical_address lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let blit_to_bytes t ~src_off:logical_address buf ~dst_off ~len =
  if len < 0 || dst_off < 0 || dst_off > Bytes.length buf - len then
    invalid_arg "Cachet.blit_to_bytes";
  let off = logical_address land ((1 lsl t.pagesize) - 1) in
  if is_aligned off && (1 lsl t.pagesize) - off >= len then begin
    match load t ~len logical_address with
    | None -> out_of_bounds logical_address
    | Some slice ->
        Bstr.blit_to_bytes slice.payload ~src_off:off buf ~dst_off:0 ~len
  end
  else
    for i = 0 to len - 1 do
      let v = get_uint8 t (logical_address + i) in
      Bytes.set_uint8 buf (dst_off + i) v
    done

let get_string t ~len logical_address =
  let buf = Bytes.create len in
  blit_to_bytes t ~src_off:logical_address buf ~dst_off:0 ~len;
  Bytes.unsafe_to_string buf

let get_uint16_ne t logical_address =
  let str = get_string t ~len:2 logical_address in
  String.get_uint16_ne str 0

let get_uint16_le t logical_address =
  let str = get_string t ~len:2 logical_address in
  String.get_uint16_le str 0

let get_uint16_be t logical_address =
  let str = get_string t ~len:2 logical_address in
  String.get_uint16_be str 0

let get_int16_ne t logical_address =
  let str = get_string t ~len:2 logical_address in
  String.get_int16_ne str 0

let get_int16_le t logical_address =
  let str = get_string t ~len:2 logical_address in
  String.get_int16_le str 0

let get_int16_be t logical_address =
  let str = get_string t ~len:2 logical_address in
  String.get_int16_be str 0

let get_int32_ne t logical_address =
  let str = get_string t ~len:4 logical_address in
  String.get_int32_ne str 0

let get_int32_le t logical_address =
  let str = get_string t ~len:4 logical_address in
  String.get_int32_le str 0

let get_int32_be t logical_address =
  let str = get_string t ~len:4 logical_address in
  String.get_int32_be str 0

let get_int64_ne t logical_address =
  let str = get_string t ~len:8 logical_address in
  String.get_int64_ne str 0

let get_int64_le t logical_address =
  let str = get_string t ~len:8 logical_address in
  String.get_int64_le str 0

let get_int64_be t logical_address =
  let str = get_string t ~len:8 logical_address in
  String.get_int64_be str 0

let rec get_seq t logical_address () =
  match load t logical_address with
  | Some { offset; payload; length; _ } ->
      let off = logical_address land ((1 lsl t.pagesize) - 1) in
      let len = length - off in
      let buf = Bytes.create len in
      Bstr.blit_to_bytes payload ~src_off:off buf ~dst_off:0 ~len;
      let str = Bytes.unsafe_to_string buf in
      let next = get_seq t (offset + (1 lsl t.pagesize)) in
      Seq.Cons (str, next)
  | None -> Seq.Nil

let next t slice = load t (slice.offset + (1 lsl t.pagesize))

let naive_iter_with_len t len ~fn logical_address =
  for i = 0 to len - 1 do
    fn (get_uint8 t (logical_address + i))
  done

let iter_with_len t len ~fn logical_address =
  if len > 1 lsl t.pagesize then naive_iter_with_len t len ~fn logical_address
  else begin
    match load t logical_address with
    | Some { offset; payload; length } ->
        let off = logical_address land ((1 lsl t.pagesize) - 1) in
        let max = Int.min (length - off) len in
        for i = 0 to max - 1 do
          fn (Bstr.get_uint8 payload (off + i))
        done;
        if max < len then begin
          let logical_address = offset + (1 lsl t.pagesize) in
          match load t logical_address with
          | Some { payload; length; _ } ->
              if len - max > length then
                out_of_bounds (logical_address + (len - max - 1));
              for i = 0 to len - max - 1 do
                fn (Bstr.get_uint8 payload i)
              done
          | None -> out_of_bounds logical_address
        end
    | None -> out_of_bounds logical_address
  end

let iter t ?len ~fn logical_address =
  match len with
  | Some len -> iter_with_len t len ~fn logical_address
  | None ->
      let rec go logical_address =
        match load t logical_address with
        | Some { offset; payload; length } ->
            let off = logical_address land ((1 lsl t.pagesize) - 1) in
            let len = length - off in
            for i = 0 to len - 1 do
              fn (Bstr.get_uint8 payload (off + i))
            done;
            go (offset + (1 lsl t.pagesize))
        | None -> ()
      in
      go logical_address

let syscalls t ~logical_address ~len =
  let pagesize = 1 lsl t.pagesize in
  let len = (logical_address land (pagesize - 1)) + len in
  let len =
    if (pagesize - 1) land len != 0 then
      (len + pagesize) land lnot (pagesize - 1)
    else len
  in
  len lsr t.pagesize
