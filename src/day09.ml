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
end

module Common = struct
  module Input = Input.Make_parseable (Grid)
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve t =
    let get (row, col) =
      match t.(row).(col) with
      | v -> Some v
      | exception Invalid_argument _ -> None
    in
    let neighbors point = Int_pair.neighbors point Int_pair.right_vectors in
    let rows = List.range 0 (Array.length t) in
    let columns = List.range 0 (Array.length t.(0)) in
    List.filter_map (List.cartesian_product rows columns) ~f:(fun coords ->
        let point = Option.value_exn (get coords) in
        let neighbors = neighbors coords in
        let neighbors = List.filter_map neighbors ~f:get in
        match List.for_all neighbors ~f:(fun neighbor -> point < neighbor) with
        | true -> Some point
        | false -> None)
    |> List.map ~f:succ
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let test_case =
      Input.of_string {|2199943210
3987894921
9856789892
8767896789
9899965678|}
    in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 15 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
