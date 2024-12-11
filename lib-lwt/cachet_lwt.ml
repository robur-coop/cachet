open Lwt

let load t ?len logical_address =
  let cached = Cachet.is_cached t logical_address in
  let res = Cachet.load t ?len logical_address in
  if not cached then Lwt.pause () >|= fun () -> res else Lwt.return res

let get_uint8 t logical_address =
  let res = Cachet.get_uint8 t logical_address in
  Lwt.pause () >|= fun () -> res

let get_int8 t logical_address =
  let res = Cachet.get_int8 t logical_address in
  Lwt.pause () >|= fun () -> res

let is_aligned x = x land ((1 lsl 2) - 1) == 0
let[@inline never] out_of_bounds offset = raise (Cachet.Out_of_bounds offset)

let blit_to_bytes t ~src_off:logical_address buf ~dst_off ~len =
  if len < 0 || dst_off < 0 || dst_off > Bytes.length buf - len then
    invalid_arg "Cachet_lwt.blit_to_bytes";
  let pagesize = Cachet.pagesize t in
  let off = logical_address land (pagesize - 1) in
  if is_aligned off && pagesize - off >= len then
    load t ~len logical_address >|= function
    | None -> out_of_bounds logical_address
    | Some slice ->
        Cachet.Bstr.blit_to_bytes slice.payload ~src_off:off buf ~dst_off:0 ~len
  else
    let rec go idx =
      if idx >= len then Lwt.return_unit
      else begin
        get_uint8 t (logical_address + idx) >>= fun v ->
        Bytes.set_uint8 buf (dst_off + idx) v;
        go (succ idx)
      end
    in
    go 0

let get_string t ~len logical_address =
  let buf = Bytes.create len in
  blit_to_bytes t ~src_off:logical_address buf ~dst_off:0 ~len >|= fun () ->
  Bytes.unsafe_to_string buf

open Lwt.Syntax

let get_uint16_ne t logical_address =
  let+ str = get_string t ~len:2 logical_address in
  String.get_uint16_ne str 0

let get_uint16_le t logical_address =
  let+ str = get_string t ~len:2 logical_address in
  String.get_uint16_le str 0

let get_uint16_be t logical_address =
  let+ str = get_string t ~len:2 logical_address in
  String.get_uint16_be str 0

let get_int16_ne t logical_address =
  let+ str = get_string t ~len:2 logical_address in
  String.get_int16_ne str 0

let get_int16_le t logical_address =
  let+ str = get_string t ~len:2 logical_address in
  String.get_int16_le str 0

let get_int16_be t logical_address =
  let+ str = get_string t ~len:2 logical_address in
  String.get_int16_be str 0

let get_int32_ne t logical_address =
  let+ str = get_string t ~len:4 logical_address in
  String.get_int32_ne str 0

let get_int32_le t logical_address =
  let+ str = get_string t ~len:4 logical_address in
  String.get_int32_le str 0

let get_int32_be t logical_address =
  let+ str = get_string t ~len:4 logical_address in
  String.get_int32_be str 0

let get_int64_ne t logical_address =
  let+ str = get_string t ~len:8 logical_address in
  String.get_int64_ne str 0

let get_int64_le t logical_address =
  let+ str = get_string t ~len:8 logical_address in
  String.get_int64_le str 0

let get_int64_be t logical_address =
  let+ str = get_string t ~len:8 logical_address in
  String.get_int64_be str 0

let next t slice =
  let pagesize = Cachet.pagesize t in
  load t (slice.Cachet.offset + (1 lsl pagesize))
