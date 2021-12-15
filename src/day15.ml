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

  let project_value value ~repetitions = ((value - 1 + repetitions) % 9) + 1

  let get t (row, col) =
    let rows = Array.length t in
    let cols = Array.length t.(0) in
    let row_repetitions = row / rows in
    let col_repetitions = col / cols in
    let original_row_index = row % rows in
    let original_col_index = col % cols in
    t.(original_row_index).(original_col_index)
    |> project_value ~repetitions:col_repetitions
    |> project_value ~repetitions:row_repetitions
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Grid)
  module Output = Int
  module Heap = Hash_heap.Make (Int_pair)

  let solve input ~repetitions =
    let rows = Array.length input * repetitions in
    let cols = Array.length input.(0) * repetitions in
    let compare_distance a b =
      match a, b with
      | None, None -> 0
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some a, Some b -> compare a b
    in
    let heap = Heap.create ~min_size:(rows * cols) compare_distance in
    Sequence.cartesian_product (Sequence.range 0 rows) (Sequence.range 0 cols)
    |> Sequence.iter ~f:(fun coords -> Heap.push_exn heap ~key:coords ~data:None);
    Heap.replace heap ~key:(0, 0) ~data:(Some 0);
    Sequence.fold_until
      (Sequence.repeat ())
      ~init:()
      ~f:(fun () () ->
        let coords, risk = Heap.pop_with_key_exn heap in
        let risk = Option.value_exn risk in
        match Int_pair.equal coords (rows - 1, cols - 1) with
        | true -> Continue_or_stop.Stop risk
        | false ->
          let neighbor_coords =
            Int_pair.neighbors coords Int_pair.right_vectors
            |> List.filter ~f:(Heap.mem heap)
          in
          List.iter neighbor_coords ~f:(fun neighbor_coords ->
              let new_risk =
                Base.Comparable.min
                  compare_distance
                  (Heap.find_exn heap neighbor_coords)
                  (Some (risk + Grid.get input neighbor_coords))
              in
              Heap.replace heap ~key:neighbor_coords ~data:new_risk);
          Continue ())
      ~finish:(fun () -> assert false)
  ;;

  (* This solution happens to work for part 1, but it assumes that we only move
     down or to the right. *)
  let solve' input ~repetitions =
    let rows = Array.length input * repetitions in
    let cols = Array.length input.(0) * repetitions in
    let risks = Array.init rows ~f:(fun _ -> Array.init cols ~f:(const None)) in
    let get_risk (row, col) =
      let get_opt array index = Option.try_with (fun () -> Array.get array index) in
      let open Option.Let_syntax in
      let%bind row = get_opt risks row in
      let%bind risk = get_opt row col in
      risk
    in
    Sequence.cartesian_product (Sequence.range 0 rows) (Sequence.range 0 cols)
    |> Sequence.iter ~f:(fun (row, col) ->
           risks.(row).(col)
             <- Some
                  (match
                     List.filter_map [ row - 1, col; row, col - 1 ] ~f:get_risk
                     |> List.min_elt ~compare
                   with
                  | None -> 0
                  | Some v -> v + Grid.get input (row, col)));
    Option.value_exn risks.(rows - 1).(cols - 1)
  ;;
end

module Part_01 = struct
  include Common

  let solve = solve ~repetitions:1
end

module Part_02 = struct
  include Common

  let solve = solve ~repetitions:5
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

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

    let%expect_test _ =
      let test value repetitions =
        let result = Grid.project_value value ~repetitions in
        print_s [%message "" (value : int) (repetitions : int) (result : int)]
      in
      test 1 1;
      test 9 1;
      test 8 4;
      [%expect
        {|
        ((value 1) (repetitions 1) (result 2))
        ((value 9) (repetitions 1) (result 1))
        ((value 8) (repetitions 4) (result 3)) |}];
      let test row col = print_s [%sexp (Grid.get test_case (row, col) : int)] in
      test 0 10;
      test 1 12;
      [%expect {|
        2
        9 |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_02.solve test_case : Output.t)];
      [%expect {| 315 |}]
    ;;
  end)
;;
