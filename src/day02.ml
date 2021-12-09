open! Core_kernel
open! Import

module Instruction = struct
  type t = int * int -> int * int

  let parser =
    let open Angstrom in
    let direction =
      take_while1 Char.is_alpha
      |> map ~f:(fun string magnitude (h, v) ->
             match string with
             | "forward" -> h + magnitude, v
             | "down" -> h, v + magnitude
             | "up" -> h, v - magnitude
             | _ -> assert false)
    in
    let magnitude = take_while Char.is_digit |> map ~f:Int.of_string in
    map2
      (direction <* skip_while Char.is_whitespace)
      magnitude
      ~f:(fun direction magnitude -> direction magnitude)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Instruction)
  module Output = Int

  let test_case =
    Input.of_string {|forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2|}
  ;;
end

module Part_01 = struct
  include Common

  let solve directions =
    let h, v = List.fold directions ~init:(0, 0) ~f:(fun v f -> f v) in
    h * v
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 150 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
