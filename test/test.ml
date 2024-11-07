let random ?g len =
  let bstr = Bigstringaf.create len in
  for i = 0 to len - 1 do
    let chr =
      match g with
      | Some g -> Char.unsafe_chr (Random.State.bits g land 0xff)
      | None -> Char.unsafe_chr (Random.bits () land 0xff)
    in
    Bigstringaf.set bstr i chr
  done;
  bstr

let make ?cachesize ?pagesize ?g len =
  let bstr = random ?g len in
  let map () ~pos len =
    if pos < 0 || len < 0 || pos > Bigarray.Array1.dim bstr then
      Printf.ksprintf invalid_arg "map ~pos:%d %d" pos len;
    let len' = Int.min (Bigarray.Array1.dim bstr - pos) len in
    Bigarray.Array1.sub bstr pos len'
  in
  (Cachet.make ?cachesize ?pagesize ~map (), bstr)

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun () ->
  let t, oracle = make ~cachesize:0x100 ~pagesize:0x100 0xe000 in
  let a = Cachet.get_uint8 t 0xdead in
  let b = Char.code (Bigstringaf.get oracle 0xdead) in
  Alcotest.(check int) "0xdead" a b;
  let a = Cachet.get_string t 0xdead ~len:10 in
  let b = Bigstringaf.substring oracle ~off:0xdead ~len:10 in
  Alcotest.(check string) "0xdead" a b;
  let a = Cachet.get_string t 0xdea0 ~len:10 in
  let b = Bigstringaf.substring oracle ~off:0xdea0 ~len:10 in
  Alcotest.(check string) "0xdea0" a b;
  let a = Cachet.get_seq t 0 in
  let b = Bigstringaf.to_string oracle in
  let a = List.of_seq a in
  let a = String.concat "" a in
  Alcotest.(check string) "all" a b

let () = Alcotest.run "chat" [ ("simple", [ test01 ]) ]
