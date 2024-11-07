# Cachet, a simple cache system for `mmap`

Cachet is a small library that provides a simple cache system for page-by-page
read access on a block device. The cache system requires a map function, which
can correspond to [Unix.map_file].

Here's a simple example using `Unix.map_file`:
```ocaml
let shared = true
let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let map fd ~pos len =
  let stat = Unix.fstat fd in
  let len = Int.min len (stat.Unix.st_size - pos) in
  if pos < stat.Unix.st_size
  then let barr = Unix.map_file fd ~pos:(Int64.of_int pos)
         Bigarray.char Bigarray.c_layout shared [| len |] in
       Bigarray.array1_of_genarray barr
  else empty

external getpagesize : unit -> int = "unix_getpagesize" [@noalloc]

let () =
  let fd = Unix.openfile "disk.img" Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let cache = Cachet.make ~pagesize:(getpagesize ()) ~map fd in
  let seq = Cachet.get_seq cache 0 in
  ...
```
