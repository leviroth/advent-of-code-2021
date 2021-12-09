open! Core_kernel
open! Import

module Instruction = struct
  type t =
    { direction : [ `Forward | `Up | `Down ]
    ; magnitude : int
    }

  let parser =
    let open Angstrom in
    let direction =
      take_while1 Char.is_alpha
      |> map ~f:(fun string ->
             match string with
             | "forward" -> `Forward
             | "down" -> `Down
             | "up" -> `Up
             | _ -> assert false)
    in
    let magnitude = take_while Char.is_digit |> map ~f:Int.of_string in
    map2
      (direction <* skip_while Char.is_whitespace)
      magnitude
      ~f:(fun direction magnitude -> { direction; magnitude })
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
    let h, v =
      List.fold
        directions
        ~init:(0, 0)
        ~f:(fun (h, v) ({ direction; magnitude } : Instruction.t) ->
          match direction with
          | `Forward -> h + magnitude, v
          | `Down -> h, v + magnitude
          | `Up -> h, v - magnitude)
    in
    h * v
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 150 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve directions =
    let h, v, _aim =
      List.fold
        directions
        ~init:(0, 0, 0)
        ~f:(fun (h, v, aim) ({ direction; magnitude } : Instruction.t) ->
          match direction with
          | `Forward -> h + magnitude, v + (aim * magnitude), aim
          | `Down -> h, v, aim + magnitude
          | `Up -> h, v, aim - magnitude)
    in
    h * v
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 900 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
