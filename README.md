# Cachet, a simple cache system for `mmap`

Cachet is a small library that provides a simple cache system for page-by-page
read access on a block device. The cache system requires a map function, which
can correspond to [Unix.map_file][unix-map-file].

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

external getpagesize : unit -> int = "unix_getpagesize" [@@noalloc]

let () =
  let fd = Unix.openfile "disk.img" Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let cache = Cachet.make ~pagesize:(getpagesize ()) ~map fd in
  let seq = Cachet.get_seq cache 0 in
  ...
```

## Cachet and schedulers

Cachet is designed to treat the `map` function as **atomic**. In other words: a
unit of work that is indivisible and guaranteed to be executed as a single,
coherent, and uninterrupted operation. Therefore, the `load` function (used to
load a page) cannot be more cooperative (and give other tasks the opportunity to
run) than it already is.

Using Cachet with a scheduler requires addressing two issues:
1) enabling cooperation **after** a page has been loaded
2) the possibility of parallel loading of the page to ensure that other tasks
   can be executed

For the first point, with regard to Lwt or Async, it's essentially a question of
potentially adding `Lwt.pause` or `Async.yield` after using `Cachet.load` (or
the user-friendly functions):
```ocaml
let () = Lwt_main.run begin
  let page = Cachet.load cache 0xdead in
  let* () = Lwt.pause () in
  ... end
```

For the second point, only OCaml 5 and effects can answer this issue by using an
effect which will notify the scheduler to read the page **in parallel**.
```ocaml
(* see [man 3 pread] *)
let map fd ~pos len = Effect.perform (Scheduler.Pread (fd, pos, len))

let () = Scheduler.run begin fun () ->
    let fd = Unix.openfile "disk.img" Unix.[ O_RDONLY ] 0o644 in
    let finally () = Unix.close fd in
    Fun.protect ~finally @@ fun () ->
    let cache = Cachet.make ~pagesize:(getpagesize ()) ~map fd in
    let page = Cachet.load cache 0xdead in
    ...
  end
```

Note that this is only effective if the page is read **in parallel**. If this is
not the case, adding a cooperation point as you could do with Lwt/Async is
enough. Reading a page remains **atomic** and allowing other tasks to run at
the same time as this reading implies that the latter must necessarily be done
in parallel (via a `Thread` or a `Domain`).

Finally, the Cachet documentation specifies how many pages we would need to read
to obtain the requested value. As a result, it's up to the user to know where
the cooperation point should be placed and whether it makes sense to use, for
example, `get_string` or just use `load` interspersed with cooperation points.

## Cachet and write operations

It is possible to have write access (only for so-called atomic data) on a block
device for which reading is done by `mmap` and writing will only be effective
via `msync`. `Cachet_wr` provides an implementation of a _write pipeline_ and
ensures consistency between writes and reads.

Finally, writes can be made effective via the `commit` function (which flushes
the entire write pipeline) or `persist` (which flushes part of the pipeline
according to the area requested by the user). It is during these operations
that the `writev` function is actually called, which should be equivalent to
`pwrite(3P)`+`msync(3P)`.

[unix-map-file]: https://ocaml.org/manual/5.2/api/Unix.html#1_Mappingfilesintomemory
