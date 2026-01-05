type sign = Unsigned | Signed
type endian = Le | Be | Ne

type 'a v =
  | Vi8 : sign -> int v
  | Vi16 : sign * endian -> int v
  | Vi32 : endian -> int32 v
  | Vi64 : endian -> int64 v
  | Vi128 : string v

type value = Value : 'a v * 'a -> value

let length_of_value : type a. a v -> int = function
  | Vi8 _ -> 1
  | Vi16 _ -> 2
  | Vi32 _ -> 4
  | Vi64 _ -> 8
  | Vi128 -> 16

let unsafe_value_into_bytes : type a. ?off:int -> bytes -> a v -> a -> unit =
 fun ?(off = 0) buf k v ->
  match k with
  | Vi8 Unsigned -> Bytes.set_uint8 buf off v
  | Vi8 Signed -> Bytes.set_int8 buf off v
  | Vi16 (Unsigned, Le) -> Bytes.set_uint16_le buf off v
  | Vi16 (Unsigned, Be) -> Bytes.set_uint16_be buf off v
  | Vi16 (Unsigned, Ne) -> Bytes.set_uint16_ne buf off v
  | Vi16 (Signed, Le) -> Bytes.set_int16_le buf off v
  | Vi16 (Signed, Be) -> Bytes.set_int16_be buf off v
  | Vi16 (Signed, Ne) -> Bytes.set_int16_ne buf off v
  | Vi32 Le -> Bytes.set_int32_le buf off v
  | Vi32 Be -> Bytes.set_int32_be buf off v
  | Vi32 Ne -> Bytes.set_int32_ne buf off v
  | Vi64 Le -> Bytes.set_int64_le buf off v
  | Vi64 Be -> Bytes.set_int64_be buf off v
  | Vi64 Ne -> Bytes.set_int64_ne buf off v
  | Vi128 -> Bytes.blit_string v 0 buf off 16

let unsafe_value_into_bstr : type a. ?off:int -> Bstr.t -> a v -> a -> unit =
 fun ?(off = 0) buf k v ->
  match k with
  | Vi8 Unsigned -> Bstr.set_uint8 buf off v
  | Vi8 Signed -> Bstr.set_int8 buf off v
  | Vi16 (Unsigned, Le) -> Bstr.set_uint16_le buf off v
  | Vi16 (Unsigned, Be) -> Bstr.set_uint16_be buf off v
  | Vi16 (Unsigned, Ne) -> Bstr.set_uint16_ne buf off v
  | Vi16 (Signed, Le) -> Bstr.set_int16_le buf off v
  | Vi16 (Signed, Be) -> Bstr.set_int16_be buf off v
  | Vi16 (Signed, Ne) -> Bstr.set_int16_ne buf off v
  | Vi32 Le -> Bstr.set_int32_le buf off v
  | Vi32 Be -> Bstr.set_int32_be buf off v
  | Vi32 Ne -> Bstr.set_int32_ne buf off v
  | Vi64 Le -> Bstr.set_int64_le buf off v
  | Vi64 Be -> Bstr.set_int64_be buf off v
  | Vi64 Ne -> Bstr.set_int64_ne buf off v
  | Vi128 -> Bstr.blit_from_string v ~src_off:0 buf ~dst_off:off ~len:16

let value_to_string k v =
  let len = length_of_value k in
  let buf = Bytes.create len in
  unsafe_value_into_bytes ~off:0 buf k v;
  Bytes.unsafe_to_string buf

type 'fd writev = 'fd -> pos:int -> Bstr.t list -> unit

type 'fd t = {
    cache: 'fd Cachet.t
  ; pagesize: int
  ; pipeline: (int * value) Dllist.t
  ; mutable areas: Diet.t
  ; fd: 'fd
  ; map: 'fd Cachet.map
  ; writev: 'fd writev
}

(* Counter Trailing Zero *)
let unsafe_ctz n =
  let t = ref 1 in
  let r = ref 0 in
  while n land !t = 0 do
    t := !t lsl 1;
    incr r
  done;
  !r

let make ?cachesize ?(pagesize = 1 lsl 12) ~map ~writev fd =
  let cache = Cachet.make ?cachesize ~pagesize ~map fd in
  let pipeline = Dllist.create () in
  let pagesize = unsafe_ctz pagesize in
  let areas = Diet.empty in
  { cache; pagesize; pipeline; areas; fd; map; writev }

let unroll : type a. 'fd t -> at:int -> a v -> a =
 fun t ~at k ->
  let buf = Bytes.make (16 * 3) '\000' in
  let len = length_of_value k in
  Cachet.blit_to_bytes t.cache ~src_off:at buf ~dst_off:16 ~len;
  let a = at - 16 and b = at + 16 in
  let fn node =
    let at', Value (k, v) = Dllist.data node in
    if at' >= a && at' < b then
      let roff = if at' >= at then 16 + (at' - at) else 16 - (at - at') in
      unsafe_value_into_bytes ~off:roff buf k v
  in
  Dllist.iter fn t.pipeline;
  match k with
  | Vi8 Unsigned -> Bytes.get_uint8 buf 16
  | Vi8 Signed -> Bytes.get_int8 buf 16
  | Vi16 (Unsigned, Le) -> Bytes.get_uint16_le buf 16
  | Vi16 (Unsigned, Be) -> Bytes.get_uint16_be buf 16
  | Vi16 (Unsigned, Ne) -> Bytes.get_uint16_ne buf 16
  | Vi16 (Signed, Le) -> Bytes.get_int16_le buf 16
  | Vi16 (Signed, Be) -> Bytes.get_int16_be buf 16
  | Vi16 (Signed, Ne) -> Bytes.get_int16_ne buf 16
  | Vi32 Le -> Bytes.get_int32_le buf 16
  | Vi32 Be -> Bytes.get_int32_be buf 16
  | Vi32 Ne -> Bytes.get_int32_ne buf 16
  | Vi64 Le -> Bytes.get_int64_le buf 16
  | Vi64 Be -> Bytes.get_int64_le buf 16
  | Vi64 Ne -> Bytes.get_int64_le buf 16
  | Vi128 -> Bytes.sub_string buf 16 16

let get : type a. 'fd t -> int -> a v -> a =
 fun t offset k ->
  let len = length_of_value k in
  let z = Diet.inter (Diet.singleton offset (offset + len)) t.areas in
  if Diet.is_empty z then
    match k with
    | Vi8 Unsigned -> Cachet.get_uint8 t.cache offset
    | Vi8 Signed -> Cachet.get_int8 t.cache offset
    | Vi16 (Unsigned, Le) -> Cachet.get_uint16_le t.cache offset
    | Vi16 (Unsigned, Be) -> Cachet.get_uint16_be t.cache offset
    | Vi16 (Unsigned, Ne) -> Cachet.get_uint16_ne t.cache offset
    | Vi16 (Signed, Le) -> Cachet.get_int16_le t.cache offset
    | Vi16 (Signed, Be) -> Cachet.get_int16_be t.cache offset
    | Vi16 (Signed, Ne) -> Cachet.get_int16_ne t.cache offset
    | Vi32 Le -> Cachet.get_int32_le t.cache offset
    | Vi32 Be -> Cachet.get_int32_be t.cache offset
    | Vi32 Ne -> Cachet.get_int32_ne t.cache offset
    | Vi64 Le -> Cachet.get_int64_le t.cache offset
    | Vi64 Be -> Cachet.get_int64_le t.cache offset
    | Vi64 Ne -> Cachet.get_int64_le t.cache offset
    | Vi128 -> Cachet.get_string t.cache ~len:16 offset
  else unroll t ~at:offset k

let set : type a. 'fd t -> int -> a v -> a -> unit =
 fun t off k v ->
  let len = length_of_value k in
  t.areas <- Diet.add off (off + len) t.areas;
  Dllist.add (off, Value (k, v)) t.pipeline

let is_aligned_4 x = x land ((1 lsl 2) - 1) == 0

let persist t off bstrs (Value (k, v)) =
  let str = value_to_string k v in
  let rec go src_off off =
    if src_off < String.length str then
      let idx = off lsr t.pagesize in
      let dst_off = off land ((1 lsl t.pagesize) - 1) in
      if idx < Array.length bstrs then begin
        let rem_in_page = (1 lsl t.pagesize) - dst_off in
        let rem_in_str = String.length str - src_off in
        let len = Int.min rem_in_page rem_in_str in
        Bstr.blit_from_string str ~src_off bstrs.(idx) ~dst_off ~len;
        go (src_off + len) (off + len)
      end
  in
  go 0 off

let persist t ~off ~len =
  let p0 = off lsr t.pagesize in
  let p1 = (off + len) lsr t.pagesize in
  let number_of_pages = Int.max 1 (p1 - p0) in
  let physical_address = p0 lsl t.pagesize in
  let fn idx =
    let pos = physical_address + (idx * (1 lsl t.pagesize)) in
    t.map t.fd ~pos (1 lsl t.pagesize)
  in
  let bstrs = Array.init number_of_pages fn in
  let fn node =
    let off', value = Dllist.data node in
    if off' >= off && off' + 1 <= off + len then
      persist t (off' - physical_address) bstrs value
  in
  Dllist.iter fn t.pipeline

(* [commit] is a little complex because the pages we want to update do not
   necessarily follow each other. We therefore use a [hashtbl] to keep the
   pages, and our [hashtbl] can grow if data is located on two pages. Next, we
   try to "coalesce" the modified pages in order to economise on our [writev].
   We then apply the changes. *)

type chunk = { off: int; len: int; rchunks: Bstr.t list }

let commit t =
  let fn node acc =
    let logical_address, _ = Dllist.data node in
    ((logical_address lsr t.pagesize) lsl t.pagesize) :: acc
  in
  let ps = Dllist.fold fn t.pipeline [] in
  let ps = List.sort_uniq Int.compare ps in
  let tbl = Hashtbl.create (List.length ps) in
  let fn physical_address =
    let bstr = t.map t.fd ~pos:physical_address (1 lsl t.pagesize) in
    Hashtbl.add tbl physical_address bstr
  in
  List.iter fn ps;
  let fn node =
    let logical_address, Value (k, v) = Dllist.data node in
    let len = length_of_value k in
    let off = logical_address land ((1 lsl t.pagesize) - 1) in
    if (1 lsl t.pagesize) - off >= len then
      let physical_address = (logical_address lsr t.pagesize) lsl t.pagesize in
      let bstr = Hashtbl.find tbl physical_address in
      unsafe_value_into_bstr ~off bstr k v
    else
      let p0 = logical_address lsr t.pagesize in
      let p1 = p0 + 1 in
      let p0 = p0 lsl t.pagesize in
      let p1 = p1 lsl t.pagesize in
      let bstr0 = Hashtbl.find tbl p0 in
      let bstr1 =
        match Hashtbl.find_opt tbl p1 with
        | Some bstr1 -> bstr1
        | None ->
            let bstr1 = t.map t.fd ~pos:p1 (1 lsl t.pagesize) in
            Hashtbl.add tbl p1 bstr1; bstr1
      in
      let str = value_to_string k v in
      let pre = (1 lsl t.pagesize) - off in
      let rem = len - pre in
      Bstr.blit_from_string str ~src_off:0 bstr0 ~dst_off:off ~len:pre;
      Bstr.blit_from_string str ~src_off:pre bstr1 ~dst_off:0 ~len:rem
  in
  Dllist.iter fn t.pipeline;
  Dllist.clear t.pipeline;
  t.areas <- Diet.empty;
  let fn p bstr acc = (p, bstr) :: acc in
  let ps = Hashtbl.fold fn tbl [] in
  let ps = List.sort (* _uniq? *) (fun (a, _) (b, _) -> Int.compare a b) ps in
  let ps =
    List.map
      (fun (off, chunk) -> { off; len= 1 lsl t.pagesize; rchunks= [ chunk ] })
      ps
  in
  let coalesce (acc, curr) next =
    if curr.off + curr.len == next.off then
      let curr =
        {
          curr with
          len= curr.len + (1 lsl t.pagesize)
        ; rchunks= List.hd next.rchunks :: curr.rchunks
        }
      in
      (acc, curr)
    else (curr :: acc, next)
  in
  match ps with
  | [] -> ()
  | p :: ps ->
      let ps, p = List.fold_left coalesce ([], p) ps in
      let fn { off; len; rchunks; _ } =
        (* NOTE(dinosaure): with [miou], it's possible to replace [t.writev] by an
       effect. In that case, we are suspended **before** [t.writev] if we [miou]
       would like to continue (and really do our [t.writev]), we invalidate what
       we just wrote and this invalidation is done without interruption!

       In other words, it's better to invalidate **after** than before. *)
        t.writev t.fd ~pos:off (List.rev rchunks);
        Cachet.invalidate t.cache ~off ~len
      in
      List.iter fn (p :: ps)
