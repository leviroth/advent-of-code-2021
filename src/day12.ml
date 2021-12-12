open! Core_kernel
open! Import

module Cave = struct
  type t = String.t [@@deriving sexp]

  let is_small (t : t) = Char.is_lowercase (String.get t 0)

  let parser =
    let open Angstrom in
    take_while1 Char.is_alpha
  ;;
end

module Path = struct
  type t = Cave.t * Cave.t [@@deriving sexp]

  let parser =
    let open Angstrom in
    both (Cave.parser <* char '-') Cave.parser
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Path)
  module Output = Int

  let make_adjacency_lists t =
    let t_reversed = List.map t ~f:Tuple2.swap in
    String.Map.of_alist_multi (t @ t_reversed)
  ;;
end

module Part_01 = struct
  include Common

  let solve input =
    let adjacency_lists = make_adjacency_lists input in
    let rec dfs cave visited =
      match cave with
      | "end" -> 1
      | _ ->
        let neighbors =
          Map.find_exn adjacency_lists cave
          |> List.filter ~f:(fun cave -> not (Set.mem visited cave))
        in
        List.sum
          (module Int)
          neighbors
          ~f:(fun cave ->
            let visited =
              match Cave.is_small cave with
              | false -> visited
              | true -> Set.add visited cave
            in
            dfs cave visited)
    in
    dfs "start" (String.Set.singleton "start")
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]

let%test_module _ =
  (module struct
    open Common

    let test_case = Input.of_string {|start-A
start-b
A-c
A-b
b-d
A-end
b-end|}

    let%expect_test _ =
      let adjacency_lists = make_adjacency_lists test_case in
      print_s [%sexp (adjacency_lists : string list String.Map.t)];
      [%expect
        {|
        ((A (c b end start)) (b (d end start A)) (c (A)) (d (b)) (end (A b))
         (start (A b))) |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : int)];
      [%expect {| 10 |}]
    ;;
  end)
;;
