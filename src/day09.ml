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

  let get t (row, col) =
    match t.(row).(col) with
    | v -> Some v
    | exception Invalid_argument _ -> None
  ;;

  let find_low_points t =
    let neighbors point = Int_pair.neighbors point Int_pair.right_vectors in
    let rows = List.range 0 (Array.length t) in
    let columns = List.range 0 (Array.length t.(0)) in
    List.filter (List.cartesian_product rows columns) ~f:(fun coords ->
        let point = Option.value_exn (get t coords) in
        let neighbors = neighbors coords in
        let neighbors = List.filter_map neighbors ~f:(get t) in
        List.for_all neighbors ~f:(fun neighbor -> point < neighbor))
  ;;
end

module Part_01 = struct
  include Common

  let solve t =
    find_low_points t
    |> List.map ~f:(fun coords -> Option.value_exn (get t coords) + 1)
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

module Part_02 = struct
  include Common

  let count_basin t coords =
    let visited = Int_pair.Hash_set.create () in
    let queue = Queue.create () in
    let enqueue_if_not_visited coords =
      match Hash_set.mem visited coords with
      | true -> ()
      | false ->
        Hash_set.add visited coords;
        Queue.enqueue queue coords
    in
    enqueue_if_not_visited coords;
    while not (Queue.is_empty queue) do
      let current = Queue.dequeue_exn queue in
      List.iter (Int_pair.neighbors current Int_pair.right_vectors) ~f:(fun coords ->
          match get t coords with
          | None -> ()
          | Some v ->
            (match v with
            | 9 -> ()
            | _ -> enqueue_if_not_visited coords))
    done;
    Hash_set.length visited
  ;;

  let solve t =
    find_low_points t
    |> List.map ~f:(count_basin t)
    |> List.sort ~compare:(Comparable.reverse compare)
    |> fun l -> List.take l 3 |> List.reduce_exn ~f:( * )
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
    [%expect {| 1134 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
