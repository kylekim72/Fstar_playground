module TIP_8

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

val tip_eight (k m n: my_nat) : Lemma (ensures sub (add k m) (add k n) == sub m n)

let rec tip_eight k m n =
  match k with
  | Z -> ()
  | S k' -> tip_eight k' m n