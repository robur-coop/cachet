module Bstr = struct
  include Bstr

  external of_bigstring : Bstr.t -> t = "%identity"

  (* TODO(dinosaure): or use [memchr]? *)
  let exists p bstr =
    let res = ref false in
    let idx = ref 0 in
    while
      !idx < Bstr.length bstr
      &&
      (res := p (Bstr.get bstr !idx);
       !res)
    do
      incr idx
    done;
    !res
end

let invalid_argf fmt = Format.kasprintf invalid_arg fmt

external hash : (int32[@unboxed]) -> int -> (int32[@unboxed])
  = "cachet_hash_mix_intnat" "caml_hash_mix_intnat"
[@@noalloc]

let hash h d = Int32.to_int (hash h d)

type slice = { offset: int; length: int; payload: Bstr.t }

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

and 'fd map = 'fd -> pos:int -> int -> Bstr.t

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

let is_aligned_4 x = x land ((1 lsl 2) - 1) == 0

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
  if is_aligned_4 off && (1 lsl t.pagesize) - off >= len then begin
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
