open! Core_kernel
open! Import

module Input = Input.Make_parseable (struct
  type t = int list

  let parser = Angstrom.(sep_by (char ',') parse_int)
end)

module Common = struct
  module Input = Input
  module Output = Int

  let solve input ~fuel_cost =
    let min = List.reduce_exn input ~f:min in
    let max = List.reduce_exn input ~f:max in
    List.map
      (List.range ~stop:`inclusive min max)
      ~f:(fun position ->
        List.sum
          (module Int)
          input
          ~f:(fun crab ->
            let distance = abs (position - crab) in
            fuel_cost distance))
    |> List.min_elt ~compare
    |> Option.value_exn
  ;;
end

module Part_01 = struct
  include Common

  let solve = solve ~fuel_cost:Fn.id

  let%expect_test _ =
    let test_case = Input.of_string "16,1,2,0,4,2,7,1,2,14" in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 37 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve =
    let triangle_number n =
      match n with
      | 0 -> 0
      | _ -> n * (n + 1) / 2
    in
    solve ~fuel_cost:triangle_number
  ;;

  let%expect_test _ =
    let test_case = Input.of_string "16,1,2,0,4,2,7,1,2,14" in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 168 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
