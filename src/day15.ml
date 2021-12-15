open! Core_kernel
open! Import

module Grid = struct
  type t = int array array [@@deriving sexp]

  let parser =
    let open Angstrom in
    let one =
      many1 (satisfy Char.is_digit)
      |> map ~f:(fun l ->
             List.map l ~f:(fun c -> Char.to_int c - Char.to_int '0') |> Array.of_list)
    in
    sep_by1 end_of_line one |> map ~f:Array.of_list
  ;;

  let print t =
    Array.iter t ~f:(fun array ->
        Array.iter array ~f:(printf "%d");
        printf "\n")
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Grid)
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve input =
    let risks = Array.map input ~f:(Array.map ~f:(const None)) in
    let get_risk (row, col) =
      let get_opt array index = Option.try_with (fun () -> Array.get array index) in
      let open Option.Let_syntax in
      let%bind row = get_opt risks row in
      let%bind risk = get_opt row col in
      risk
    in
    let rows = Array.length input in
    let cols = Array.length input.(0) in
    Sequence.cartesian_product (Sequence.range 0 rows) (Sequence.range 0 cols)
    |> Sequence.iter ~f:(fun (row, col) ->
           risks.(row).(col)
             <- Some
                  (match
                     List.filter_map [ row - 1, col; row, col - 1 ] ~f:get_risk
                     |> List.min_elt ~compare
                   with
                  | None -> 0
                  | Some v -> v + input.(row).(col)));
    Option.value_exn risks.(rows - 1).(cols - 1)
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]

let%test_module _ =
  (module struct
    open Common

    let test_case =
      Input.of_string
        {|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581|}
    ;;

    let%expect_test _ =
      print_s [%sexp (test_case : Grid.t)];
      [%expect
        {|
        ((1 1 6 3 7 5 1 7 4 2) (1 3 8 1 3 7 3 6 7 2) (2 1 3 6 5 1 1 3 2 8)
         (3 6 9 4 9 3 1 5 6 9) (7 4 6 3 4 1 7 1 1 1) (1 3 1 9 1 2 8 1 3 7)
         (1 3 5 9 9 1 2 4 2 1) (3 1 2 5 4 2 1 6 3 9) (1 2 9 3 1 3 8 5 2 1)
         (2 3 1 1 9 4 4 5 8 1)) |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : Output.t)];
      [%expect {| 40 |}]
    ;;
  end)
;;
