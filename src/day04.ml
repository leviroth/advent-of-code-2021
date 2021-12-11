open! Core_kernel
open! Import

module Board = struct
  type t = int array array [@@deriving sexp]

  let dimension = 5

  let line =
    let open Angstrom in
    count dimension (skip_many (char ' ') *> parse_int <* skip_many (char ' '))
    |> map ~f:Array.of_list
  ;;

  let parser =
    let open Angstrom in
    let line =
      count dimension (skip_many (char ' ') *> parse_int <* skip_many (char ' '))
      |> map ~f:Array.of_list
    in
    let board =
      count dimension (line <* (end_of_line *> return () <|> end_of_input))
      |> map ~f:Array.of_list
    in
    board
  ;;

  let%expect_test _ =
    let test_case =
      {|22 13 17 11  0
       8  2 23  4 24
      21  9 14 16  7
       6 10  3 18  5
       1 12 20 15 19|}
    in
    let board =
      Angstrom.parse_string parser test_case ~consume:All |> Result.ok_or_failwith
    in
    print_s [%sexp (board : t)];
    [%expect
      {|
      ((22 13 17 11 0) (8 2 23 4 24) (21 9 14 16 7) (6 10 3 18 5) (1 12 20 15 19)) |}]
  ;;

  let winning_sets (t : t) =
    let scan_in_direction (row, col) (d_row, d_col) =
      (* We assume we can always take 5 steps. *)
      List.range 0 dimension
      |> List.map ~f:(fun step -> t.(row + (step * d_row)).(col + (step * d_col)))
      |> Int.Set.of_list
    in
    let rows =
      List.map (List.range 0 dimension) ~f:(fun row -> scan_in_direction (row, 0) (0, 1))
    in
    let cols =
      List.map (List.range 0 dimension) ~f:(fun col -> scan_in_direction (0, col) (1, 0))
    in
    rows @ cols
  ;;

  let has_win t ~drawn_numbers =
    let drawn_numbers = Int.Set.of_list drawn_numbers in
    List.exists (winning_sets t) ~f:(Set.is_subset ~of_:drawn_numbers)
  ;;

  let%expect_test _ =
    let test_case =
      {|22 13 17 11  0
       8  2 23  4 24
      21  9 14 16  7
       6 10  3 18  5
       1 12 20 15 19|}
    in
    let board =
      Angstrom.parse_string parser test_case ~consume:All |> Result.ok_or_failwith
    in
    (* Vertical win*)
    print_s [%sexp (has_win board ~drawn_numbers:[ 13; 2; 9; 10; 12 ] : bool)];
    [%expect {| true |}];
    (* Horizontal win*)
    print_s [%sexp (has_win board ~drawn_numbers:[ 21; 9; 14; 16; 7 ] : bool)];
    [%expect {| true |}];
    (* No win*)
    print_s [%sexp (has_win board ~drawn_numbers:[ 13; 2; 9; 10; 1 ] : bool)];
    [%expect {| false |}]
  ;;
end

module Game = struct
  type t =
    { numbers : int list
    ; boards : Board.t list
    }

  let parser =
    let open Angstrom in
    let numbers = sep_by1 (char ',') parse_int in
    map2
      (numbers <* end_of_line <* end_of_line)
      (sep_by1 end_of_line Board.parser)
      ~f:(fun numbers boards -> { numbers; boards })
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Game)
  module Output = Int
end

module Part_01 = struct
  include Common

  let score_game board ~last_drawn_number ~drawn_numbers =
    let all_numbers =
      List.fold (Board.winning_sets board) ~init:Int.Set.empty ~f:Int.Set.union
    in
    let filtered_numbers = List.fold drawn_numbers ~init:all_numbers ~f:Set.remove in
    let total = List.sum (module Int) (Set.to_list filtered_numbers) ~f:Fn.id in
    last_drawn_number * total
  ;;

  let solve ({ numbers; boards } : Input.t) =
    List.fold_until
      numbers
      ~init:[]
      ~f:(fun numbers number ->
        let drawn_numbers = number :: numbers in
        match List.find boards ~f:(Board.has_win ~drawn_numbers) with
        | None -> Continue drawn_numbers
        | Some board -> Stop (score_game board ~last_drawn_number:number ~drawn_numbers))
      ~finish:(fun _ -> assert false)
  ;;

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7|}
    in
    printf "%d\n" (solve test_case);
    [%expect {| 4512 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
