(* (c) MirageOS developers *)

type t = Empty | Node : node -> t
and node = { x: int; y: int; l: t; r: t; h: int }

let empty = Empty
let is_empty = function Empty -> true | _ -> false
let height = function Empty -> 0 | Node n -> n.h

let create x y l r =
  let h = Int.max (height l) (height r) + 1 in
  Node { x; y; l; r; h }

let rec node x y l r =
  let hl = height l and hr = height r in
  if hl > hr + 2 then begin
    match l with
    | Empty -> assert false
    | Node { x= lx; y= ly; l= ll; r= lr; _ } -> (
        if height ll >= height lr then node lx ly ll (node x y lr r)
        else
          match lr with
          | Empty -> assert false
          | Node { x= lrx; y= lry; l= lrl; r= lrr; _ } ->
              node lrx lry (node lx ly ll lrl) (node x y lrr r))
  end
  else if hr > hl + 2 then begin
    match r with
    | Empty -> assert false
    | Node { x= rx; y= ry; l= rl; r= rr; _ } -> (
        if height rr >= height rl then node rx ry (node x y l rl) rr
        else
          match rl with
          | Empty -> assert false
          | Node { x= rlx; y= rly; l= rll; r= rlr; _ } ->
              node rlx rly (node x y l rll) (node rx ry rlr rr))
  end
  else create x y l r

let rec splitMax = function
  | { x; y; l; r= Empty; _ } -> (x, y, l)
  | { r= Node r; _ } as n ->
      let u, v, r' = splitMax r in
      (u, v, node n.x n.y n.l r')

let rec splitMin = function
  | { x; y; l= Empty; r; _ } -> (x, y, r)
  | { l= Node l; _ } as n ->
      let u, v, l' = splitMin l in
      (u, v, node n.x n.y l' n.r)

let addL = function
  | { l= Empty; _ } as n -> n
  | { l= Node l; _ } as n ->
      let x', y', l' = splitMax l in
      if succ y' = n.x then { n with x= x'; l= l' } else n

let addR = function
  | { r= Empty; _ } as n -> n
  | { r= Node r; _ } as n ->
      let x', y', r' = splitMin r in
      if succ n.y = x' then { n with y= y'; r= r' } else n

let rec add x y t =
  match t with
  | Empty -> node x y Empty Empty
  (* completely to the left *)
  | Node n when y < pred n.x ->
      let l = add x y n.l in
      node n.x n.y l n.r
  (* completely to the right *)
  | Node n when succ n.y < x ->
      let r = add x y n.r in
      node n.x n.y n.l r
  (* overlap on the left only *)
  | Node n when x < n.x && y <= n.y ->
      let l = add x (pred n.x) n.l in
      let n = addL { n with l } in
      node n.x n.y n.l n.r
  (* overlap on the right only *)
  | Node n when y > n.y && x >= n.x ->
      let r = add (succ n.y) y n.r in
      let n = addR { n with r } in
      node n.x n.y n.l n.r
  (* overlap on both sides *)
  | Node n when x < n.x && y > n.y ->
      let l = add x (pred n.x) n.l in
      let r = add (succ n.y) y n.r in
      let n = addL { (addR { n with r }) with l } in
      node n.x n.y n.l n.r
  (* completely within *)
  | Node n -> Node n

let singleton x y = add x y Empty

let merge l r =
  match (l, r) with
  | l, Empty -> l
  | Empty, r -> r
  | Node l, r ->
      let x, y, l' = splitMax l in
      node x y l' r

let rec remove (x, y) t =
  match t with
  | Empty -> Empty
  (* completely to the left *)
  | Node n when y < n.x ->
      let l = remove (x, y) n.l in
      node n.x n.y l n.r
  (* completely to the right *)
  | Node n when n.y < x ->
      let r = remove (x, y) n.r in
      node n.x n.y n.l r
  (* overlap on the left only *)
  | Node n when x < n.x && y < n.y ->
      let n' = node (succ y) n.y n.l n.r in
      remove (x, pred n.x) n'
  (* overlap on the right only *)
  | Node n when y > n.y && x > n.x ->
      let n' = node n.x (pred x) n.l n.r in
      remove (succ n.y, y) n'
  (* overlap on both sides *)
  | Node n when x <= n.x && y >= n.y ->
      let l = remove (x, n.x) n.l in
      let r = remove (n.y, y) n.r in
      merge l r
  (* completely within *)
  | Node n when y = n.y -> node n.x (pred x) n.l n.r
  | Node n when x = n.x -> node (succ y) n.y n.l n.r
  | Node n ->
      assert (n.x <= pred x);
      assert (succ y <= n.y);
      let r = node (succ y) n.y Empty n.r in
      node n.x (pred x) n.l r

let rec fold fn t acc =
  match t with
  | Empty -> acc
  | Node n ->
      let acc = fold fn n.l acc in
      let acc = fn (n.x, n.y) acc in
      fold fn n.r acc

let diff a b = fold remove b a
let inter a b = diff a (diff a b)
