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

  let get t (row, col) =
    match t.(row).(col) with
    | v -> Some v
    | exception Invalid_argument _ -> None
  ;;

  let all_coords t =
    let rows = List.range 0 (Array.length t) in
    let columns = List.range 0 (Array.length t.(0)) in
    List.cartesian_product rows columns
  ;;

  let count_flashes t =
    let queue = Queue.create () in
    let enqueue_if_newly_flashing coords ~energy_level =
      match energy_level = 10 with
      | false -> ()
      | true -> Queue.enqueue queue coords
    in
    List.iter (all_coords t) ~f:(fun coords ->
        enqueue_if_newly_flashing coords ~energy_level:(Option.value_exn (get t coords)));
    while not (Queue.is_empty queue) do
      let current = Queue.dequeue_exn queue in
      List.iter
        (Int_pair.neighbors current Int_pair.(right_vectors @ diagonal_vectors))
        ~f:(fun ((row, col) as coords) ->
          match get t coords with
          | None -> ()
          | Some energy_level ->
            let energy_level = energy_level + 1 in
            t.(row).(col) <- energy_level;
            enqueue_if_newly_flashing coords ~energy_level)
    done;
    Array.sum (module Int) t ~f:(Array.count ~f:(fun v -> v > 9))
  ;;

  let one_step t =
    Array.iter t ~f:(Array.map_inplace ~f:(( + ) 1));
    let total = count_flashes t in
    Array.iter
      t
      ~f:
        (Array.map_inplace ~f:(fun v ->
             match v > 9 with
             | true -> 0
             | false -> v));
    total
  ;;
end

module Part_01 = struct
  include Common

  let solve t = List.sum (module Int) (List.range 0 100) ~f:(fun _ -> one_step t)

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|}
    in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 1656 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve t =
    Sequence.find_exn
      (Sequence.unfold ~init:1 ~f:(fun v -> Some (v, v + 1)))
      ~f:(fun _ -> one_step t = 100)
  ;;

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|}
    in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 195 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
