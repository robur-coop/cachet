open Crowbar

let _reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

(*
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
*)

[@@@warning "-37"]

type t =
  | Get : int * 'a Cachet_wr.v -> t
  | Set : int * 'a Cachet_wr.v * 'a -> t
  | Persist : { off: int; len: int } -> t

type value = Cachet_wr.value = Value : 'a Cachet_wr.v * 'a -> value

let pp_v : type a. a Cachet_wr.v Fmt.t =
 fun ppf v ->
  let open Cachet_wr in
  match v with
  | Vi8 Unsigned -> Fmt.string ppf "ui8"
  | Vi8 Signed -> Fmt.string ppf "i8"
  | Vi16 (Unsigned, Le) -> Fmt.string ppf "leui16"
  | Vi16 (Unsigned, Be) -> Fmt.string ppf "beui16"
  | Vi16 (Unsigned, Ne) -> Fmt.string ppf "neui16"
  | Vi16 (Signed, Le) -> Fmt.string ppf "lei16"
  | Vi16 (Signed, Be) -> Fmt.string ppf "bei16"
  | Vi16 (Signed, Ne) -> Fmt.string ppf "nei16"
  | Vi32 Le -> Fmt.string ppf "lei32"
  | Vi32 Be -> Fmt.string ppf "bei32"
  | Vi32 Ne -> Fmt.string ppf "nei32"
  | Vi64 Le -> Fmt.string ppf "lei64"
  | Vi64 Be -> Fmt.string ppf "bei64"
  | Vi64 Ne -> Fmt.string ppf "nei64"
  | Vi128 -> Fmt.string ppf "i128"

let pp_value : type a. a Cachet_wr.v -> a Fmt.t =
 fun k ppf v ->
  match k with
  | Vi8 Unsigned -> Fmt.pf ppf "ui8:%x" v
  | Vi8 Signed -> Fmt.pf ppf "i8:%x" v
  | Vi16 (Unsigned, Le) -> Fmt.pf ppf "leui16:%x" v
  | Vi16 (Unsigned, Be) -> Fmt.pf ppf "beui16:%x" v
  | Vi16 (Unsigned, Ne) -> Fmt.pf ppf "neui16:%x" v
  | Vi16 (Signed, Le) -> Fmt.pf ppf "lei16:%x" v
  | Vi16 (Signed, Be) -> Fmt.pf ppf "bei16:%x" v
  | Vi16 (Signed, Ne) -> Fmt.pf ppf "nei16:%x" v
  | Vi32 Le -> Fmt.pf ppf "lei32:%lx" v
  | Vi32 Be -> Fmt.pf ppf "bei32:%lx" v
  | Vi32 Ne -> Fmt.pf ppf "nei32:%lx" v
  | Vi64 Le -> Fmt.pf ppf "lei64:%Lx" v
  | Vi64 Be -> Fmt.pf ppf "lei64:%Lx" v
  | Vi64 Ne -> Fmt.pf ppf "lei64:%Lx" v
  | Vi128 -> Fmt.pf ppf "i128:%s" (Ohex.encode v)

let _pp ppf = function
  | Get (off, k) -> Fmt.pf ppf "get(%d, %a)" off pp_v k
  | Set (off, k, v) -> Fmt.pf ppf "set(%d, %a)" off (pp_value k) v
  | Persist { off; len } -> Fmt.pf ppf "persist(off:%d, len:%d)" off len

let pp_value ppf (Value (k, v)) = pp_value k ppf v

let set max =
  map [ range ~min:0 max; range ~min:0 15; bytes_fixed 16 ]
  @@ fun offset idx tmp ->
  match idx with
  | 0 -> Set (offset, Vi8 Unsigned, String.get_uint8 tmp 0)
  | 1 -> Set (offset, Vi8 Signed, String.get_int8 tmp 0)
  | 2 -> Set (offset, Vi16 (Unsigned, Le), String.get_uint16_le tmp 0)
  | 3 -> Set (offset, Vi16 (Unsigned, Be), String.get_uint16_be tmp 0)
  | 4 -> Set (offset, Vi16 (Unsigned, Ne), String.get_uint16_ne tmp 0)
  | 5 -> Set (offset, Vi16 (Signed, Le), String.get_int16_le tmp 0)
  | 6 -> Set (offset, Vi16 (Signed, Be), String.get_int16_be tmp 0)
  | 7 -> Set (offset, Vi16 (Signed, Ne), String.get_int16_ne tmp 0)
  | 8 -> Set (offset, Vi32 Le, String.get_int32_le tmp 0)
  | 9 -> Set (offset, Vi32 Be, String.get_int32_be tmp 0)
  | 10 -> Set (offset, Vi32 Ne, String.get_int32_ne tmp 0)
  | 11 -> Set (offset, Vi64 Le, String.get_int64_le tmp 0)
  | 12 -> Set (offset, Vi64 Be, String.get_int64_be tmp 0)
  | 13 -> Set (offset, Vi64 Ne, String.get_int64_ne tmp 0)
  | 14 -> Set (offset, Vi128, tmp)
  | _ -> assert false

let get max =
  map [ range ~min:0 max; range ~min:0 8 ] @@ fun off idx ->
  match idx with
  | 0 -> Get (off, Vi8 Unsigned)
  | 1 -> Get (off, Vi8 Signed)
  | 2 -> Get (off, Vi16 (Unsigned, Le))
  | 3 -> Get (off, Vi16 (Unsigned, Be))
  | 4 -> Get (off, Vi16 (Unsigned, Ne))
  | 5 -> Get (off, Vi16 (Signed, Le))
  | 6 -> Get (off, Vi16 (Signed, Be))
  | 7 -> Get (off, Vi16 (Signed, Ne))
  | 8 -> Get (off, Vi32 Le)
  | 9 -> Get (off, Vi32 Be)
  | 10 -> Get (off, Vi32 Ne)
  | 11 -> Get (off, Vi64 Le)
  | 12 -> Get (off, Vi64 Be)
  | 13 -> Get (off, Vi64 Ne)
  | 14 -> Get (off, Vi128)
  | _ -> assert false

let persist max =
  map [ range ~min:0 max; range ~min:0 32 ] @@ fun off pad ->
  let len = pad * 2 in
  if off + len >= max then bad_test () else Persist { off; len }

let t max = choose [ get max; set max; persist max ]

let on_bytes max actions =
  let buf = Bytes.make (max + 16) '\000' in
  let fn acc = function
    | Get (off, Vi8 Unsigned) ->
        let v = Bytes.get_uint8 buf off in
        Value (Vi8 Unsigned, v) :: acc
    | Get (off, Vi8 Signed) ->
        let v = Bytes.get_int8 buf off in
        Value (Vi8 Signed, v) :: acc
    | Get (off, Vi16 (Unsigned, Le)) ->
        let v = Bytes.get_uint16_le buf off in
        Value (Vi16 (Unsigned, Le), v) :: acc
    | Get (off, Vi16 (Unsigned, Be)) ->
        let v = Bytes.get_uint16_be buf off in
        Value (Vi16 (Unsigned, Be), v) :: acc
    | Get (off, Vi16 (Unsigned, Ne)) ->
        let v = Bytes.get_uint16_ne buf off in
        Value (Vi16 (Unsigned, Ne), v) :: acc
    | Get (off, Vi16 (Signed, Le)) ->
        let v = Bytes.get_int16_le buf off in
        Value (Vi16 (Signed, Le), v) :: acc
    | Get (off, Vi16 (Signed, Be)) ->
        let v = Bytes.get_int16_be buf off in
        Value (Vi16 (Signed, Be), v) :: acc
    | Get (off, Vi16 (Signed, Ne)) ->
        let v = Bytes.get_int16_ne buf off in
        Value (Vi16 (Signed, Ne), v) :: acc
    | Get (off, Vi32 Le) ->
        let v = Bytes.get_int32_le buf off in
        Value (Vi32 Le, v) :: acc
    | Get (off, Vi32 Be) ->
        let v = Bytes.get_int32_be buf off in
        Value (Vi32 Be, v) :: acc
    | Get (off, Vi32 Ne) ->
        let v = Bytes.get_int32_ne buf off in
        Value (Vi32 Ne, v) :: acc
    | Get (off, Vi64 Le) ->
        let v = Bytes.get_int64_le buf off in
        Value (Vi64 Le, v) :: acc
    | Get (off, Vi64 Be) ->
        let v = Bytes.get_int64_be buf off in
        Value (Vi64 Be, v) :: acc
    | Get (off, Vi64 Ne) ->
        let v = Bytes.get_int64_ne buf off in
        Value (Vi64 Ne, v) :: acc
    | Get (off, Vi128) ->
        let v = Bytes.sub_string buf off 16 in
        Value (Vi128, v) :: acc
    | Persist _ -> acc
    | Set (off, Vi8 Unsigned, v) -> Bytes.set_uint8 buf off v; acc
    | Set (off, Vi8 Signed, v) -> Bytes.set_int8 buf off v; acc
    | Set (off, Vi16 (Unsigned, Le), v) ->
        Bytes.set_uint16_le buf off v;
        acc
    | Set (off, Vi16 (Unsigned, Be), v) ->
        Bytes.set_uint16_be buf off v;
        acc
    | Set (off, Vi16 (Unsigned, Ne), v) ->
        Bytes.set_uint16_ne buf off v;
        acc
    | Set (off, Vi16 (Signed, Le), v) ->
        Bytes.set_int16_le buf off v;
        acc
    | Set (off, Vi16 (Signed, Be), v) ->
        Bytes.set_int16_be buf off v;
        acc
    | Set (off, Vi16 (Signed, Ne), v) ->
        Bytes.set_int16_ne buf off v;
        acc
    | Set (off, Vi32 Le, v) ->
        Bytes.set_int32_le buf off v;
        acc
    | Set (off, Vi32 Be, v) ->
        Bytes.set_int32_be buf off v;
        acc
    | Set (off, Vi32 Ne, v) ->
        Bytes.set_int32_ne buf off v;
        acc
    | Set (off, Vi64 Le, v) ->
        Bytes.set_int64_le buf off v;
        acc
    | Set (off, Vi64 Be, v) ->
        Bytes.set_int64_be buf off v;
        acc
    | Set (off, Vi64 Ne, v) ->
        Bytes.set_int64_ne buf off v;
        acc
    | Set (off, Vi128, v) ->
        Bytes.blit_string v 0 buf off 16;
        acc
  in
  let results = List.fold_left fn [] actions in
  (results, Bytes.unsafe_to_string buf)

let on_cachet ?(pagesize = 1 lsl 12) max actions =
  let len = (max + 16 + (pagesize - 1)) land lnot (pagesize - 1) in
  let number_of_pages = len / pagesize in
  let bstr = Bstr.make len '\000' in
  let map bstr ~pos len = Bstr.sub bstr ~off:pos ~len in
  let writev dst ~pos:dst_off srcs =
    let dst_off = ref dst_off in
    let fn src =
      Bstr.blit src ~src_off:0 dst ~dst_off:!dst_off ~len:pagesize;
      dst_off := !dst_off + pagesize
    in
    List.iter fn srcs
  in
  let t = Cachet_wr.make ~pagesize ~map ~writev ~number_of_pages bstr in
  let fn acc = function
    | Get (offset, k) ->
        let v = Cachet_wr.get t offset k in
        Value (k, v) :: acc
    | Persist { off; len } ->
        Cachet_wr.persist t ~off ~len;
        acc
    | Set (off, k, v) -> Cachet_wr.set t off k v; acc
  in
  let results = List.fold_left fn [] actions in
  Cachet_wr.commit t;
  (results, Bstr.sub_string bstr ~off:0 ~len:(max + 16))

let pp_hexdump = Hxd_string.pp Hxd.default

let () =
  add_test ~name:"cachet.wr" [ list1 (t 0x100) ] @@ fun ts ->
  (* Fmt.epr "$> @[<hov>%a@]\n%!" Fmt.(Dump.list _pp) ts; *)
  let res0, str0 = on_bytes 0x100 ts in
  let res1, str1 = on_cachet ~pagesize:(1 lsl 6) 0x100 ts in
  List.iter2 (check_eq ~pp:pp_value) res0 res1;
  check_eq ~pp:pp_hexdump str0 str1
