module TIP_6

#push-options "--fuel 1 --query_stats"

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

val tip_six (n m: my_nat) : Lemma (ensures sub n (add n m) == Z)

let rec tip_six n m =
  match n with
  | Z -> ()
  | S n' -> tip_six n' m