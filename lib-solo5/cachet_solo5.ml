let failwithf fmt = Format.kasprintf failwith fmt

open Solo5_os.Solo5

type solo5_block_info = { capacity: int64; block_size: int64 }
type file_descr = int64

external solo5_block_acquire : string -> solo5_result * int64 * solo5_block_info
  = "mirage_solo5_block_acquire"

external solo5_block_read :
  int64 -> int64 -> Bstr.t -> int -> int -> solo5_result
  = "mirage_solo5_block_read_3"

let _max_int31 = 2147483647L
let _max_int63 = 9223372036854775807L

let connect ?cachesize name =
  match solo5_block_acquire name with
  | SOLO5_R_AGAIN, _, _ -> assert false
  | SOLO5_R_EINVAL, _, _ -> failwithf "connect(%s): Invalid argument" name
  | SOLO5_R_EUNSPEC, _, _ -> failwithf "connect(%s): Unspecified error" name
  | SOLO5_R_OK, handle, info ->
      if Sys.word_size == 32 && info.capacity > _max_int31 then
        failwithf "connect(%s): Too large block device" name;
      if Sys.word_size == 64 && info.capacity > _max_int63 then
        failwithf "connect(%s): Too large block device" name;
      if Sys.word_size == 32 && info.block_size > _max_int31 then
        failwithf "connect(%s): Too large page size" name;
      if Sys.word_size == 64 && info.block_size > _max_int63 then
        failwithf "connect(%s): Too large page size" name;
      let max = Int64.to_int info.capacity in
      let pagesize = Int64.to_int info.block_size in
      let map handle ~pos _len =
        if pos > max then Bstr.empty
        else
          let len = Int.min (max - pos) pagesize in
          let raw = Bstr.create pagesize in
          Bstr.fill raw '\000';
          match solo5_block_read handle (Int64.of_int pos) raw 0 len with
          | SOLO5_R_OK -> raw
          | _ -> Bstr.empty
      in
      Cachet.make ?cachesize ~pagesize ~map handle
