module TIP_10

type my_nat =
  | Z : my_nat
  | S : my_nat -> my_nat

let rec sub (x y: my_nat) : my_nat =
  match x with
  | Z -> Z
  | S x' ->
    (match y with
      | Z -> x
      | S y' -> sub x' y')

let rec add (x y: my_nat) : my_nat =
  match x with
  | Z -> y
  | S x' -> S (add x' y)

val sub_self_is_Z (m: my_nat) : Lemma (ensures sub m m == Z)

let rec sub_self_is_Z m =
  match m with
  | Z -> ()
  | S m' -> sub_self_is_Z m'