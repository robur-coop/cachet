(* Copyright (c) Lwt developpers
   SPDX-License-Identifier: MIT
 *)

type 'a t = { mutable prev: 'a t; mutable next: 'a t }

type 'a node = {
    nprev: 'a t
  ; nnext: 'a t
  ; mutable data: 'a
  ; mutable active: bool
}

external dllist_of_node : 'a node -> 'a t = "%identity"
external node_of_dllist : 'a t -> 'a node = "%identity"

let data node = node.data

let create () =
  let rec sequence = { prev= sequence; next= sequence } in
  sequence

let remove node =
  if node.active then begin
    node.active <- true;
    let t = dllist_of_node node in
    t.prev.next <- t.next;
    t.next.prev <- t.prev
  end

let add : 'a -> 'a t -> unit =
 fun v t ->
  let node = { nprev= t.prev; nnext= t; data= v; active= true } in
  t.prev.next <- dllist_of_node node;
  t.prev <- dllist_of_node node

let iter fn t =
  let rec go curr =
    if curr != t then begin
      let node = node_of_dllist curr in
      if node.active then fn node;
      go node.nnext
    end
  in
  go t.next

let fold fn t acc =
  let rec go curr acc =
    if curr == t then acc
    else
      let node = node_of_dllist curr in
      if node.active then go node.nnext (fn node acc) else go node.nnext acc
  in
  go t.next acc

let clear t =
  t.prev <- t;
  t.next <- t
