open! Core_kernel
open! Import

module Line = struct
  type t = (int * int) * (int * int) [@@deriving sexp]

  let is_horizontal_or_vertical ((x1, y1), (x2, y2)) = x1 = x2 || y1 = y2

  let covered_points ((x1, y1), (x2, y2)) =
    let dx = x2 - x1 in
    let dy = y2 - y1 in
    let distance = max (abs dx) (abs dy) in
    let clamp v = max (-1) (min v 1) in
    let dx = clamp dx in
    let dy = clamp dy in
    List.range ~stop:`inclusive 0 distance
    |> List.map ~f:(fun step -> x1 + (step * dx), y1 + (step * dy))
  ;;

  let%expect_test _ =
    let test a b = print_s [%sexp (covered_points (a, b) : (int * int) list)] in
    test (1, 1) (1, 3);
    [%expect {| ((1 1) (1 2) (1 3)) |}];
    test (9, 7) (7, 7);
    [%expect {| ((9 7) (8 7) (7 7)) |}]
  ;;

  let parser =
    let open Angstrom in
    let point = both (parse_int <* char ',') parse_int in
    both (point <* string " -> ") point
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Line)
  module Output = Int

  let solve_filtered_lines lines =
    List.concat_map lines ~f:Line.covered_points
    |> List.fold
         ~init:Int_pair.Map.empty
         ~f:
           (Map.update ~f:(function
               | None -> 1
               | Some n -> n + 1))
    |> Map.count ~f:(fun n -> n > 1)
  ;;
end

module Part_01 = struct
  include Common

  let solve lines =
    List.filter lines ~f:Line.is_horizontal_or_vertical |> solve_filtered_lines
  ;;

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|}
    in
    printf "%d\n" (solve test_case);
    [%expect {| 5 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve = solve_filtered_lines

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|}
    in
    printf "%d\n" (solve test_case);
    [%expect {| 12 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
