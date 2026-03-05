module TIP_9

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

val tip_nine (a b c: my_nat) : Lemma (ensures sub (sub a b) c == sub a (add b c))



// let rec tip_nine a b c =
//   match a with
//   | Z -> ()
//   | S a' -> tip_nine a' b c

let rec tip_nine a b c =
  match a with
  | Z -> ()
  | S a' ->
    (match b with
      | Z -> ()
      | S b' -> tip_nine a' b' c)