module AVLInductiveSpec

(* 1. Nat type *)
type nat_t =
  | Z : nat_t
  | S : nat_t -> nat_t

(* 2. Nat operators from the first principle *)
let rec leq (x: nat_t) (y: nat_t) : bool =
  match x, y with
  | Z, _ -> true
  | S _, Z -> false
  | S x', S y' -> leq x' y'

let rec lt (x: nat_t) (y: nat_t) : bool =
  match x, y with
  | _, Z -> false
  | Z, S _ -> true
  | S x', S y' -> lt x' y'

let rec eq_nat (x: nat_t) (y: nat_t) : bool =
  match x, y with
  | Z, Z -> true
  | S x', S y' -> eq_nat x' y'
  | _, _ -> false

let rec max (x: nat_t) (y: nat_t) : nat_t =
  match x, y with
  | Z, _ -> y
  | _, Z -> x
  | S x', S y' -> S (max x' y')

(* 3. Tree datatype *)
type tree =
  | Leaf : tree
  | Node : left: tree -> value: nat_t -> height: nat_t -> right: tree -> tree

(* 4. height function *)
let height (t: tree) : nat_t =
  match t with
  | Leaf -> Z
  | Node _ _ h _ -> h

let node (l: tree) (v: nat_t) (r: tree) : tree =
  Node l v (S (max (height l) (height r))) r

(* 5. invariant *)
(* |hl - hr| <= 1 is equivalent to (hl <= hr + 1) && (hr <= hl + 1) *)
let rec is_balanced (t: tree) : bool =
  match t with
  | Leaf -> true
  | Node l _ h r ->
    let hl = height l in
    let hr = height r in
    leq hl (S hr) &&
    leq hr (S hl) &&
    eq_nat h (S (max hl hr)) &&
    is_balanced l && is_balanced r

(* 6. Rotation (right / left) *)
let rotate_right (l: tree) (v: nat_t) (r: tree) : tree =
  match l with
  | Node ll lv _ lr ->
    if leq (height lr) (height ll) then
      node ll lv (node lr v r)
    else
      (match lr with
       | Node lrl lrv _ lrr -> node (node ll lv lrl) lrv (node lrr v r)
       | Leaf -> Leaf)
  | Leaf -> Leaf

let rotate_left (l: tree) (v: nat_t) (r: tree) : tree =
  match r with
  | Node rl rv _ rr ->
    if leq (height rl) (height rr) then
      node (node l v rl) rv rr
    else
      (match rl with
       | Node rll rlv _ rlr -> node (node l v rll) rlv (node rlr rv rr)
       | Leaf -> Leaf)
  | Leaf -> Leaf

(* 7. Balance function *)
(* hl - hr > 1 (즉, hl >= hr + 2) is equivalent to leq (S (S hr)) hl *)
let balance (l: tree) (v: nat_t) (r: tree) : tree =
  let hl = height l in
  let hr = height r in
  if leq (S (S hr)) hl then rotate_right l v r
  else if leq (S (S hl)) hr then rotate_left l v r
  else node l v r

(* 8. AVL insert *)
let rec insert (x: nat_t) (t: tree) : tree =
  match t with
  | Leaf -> Node Leaf x (S Z) Leaf
  | Node l v _ r ->
    if lt x v then balance (insert x l) v r
    else if lt v x then balance l v (insert x r)
    else t

(* ======================================================== *)
(* Goal theorem                       *)
(* ======================================================== *)

let insert_preserves_balance_and_height_bound (x: nat_t) (t: tree)
  : Lemma (requires is_balanced t)
          (ensures (let h_orig = height t in
                    let h_new = height (insert x t) in
                    is_balanced (insert x t) /\
                    (h_new == h_orig \/ h_new == S h_orig)))
  = admit ()