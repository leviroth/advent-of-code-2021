open! Core_kernel
open! Import

module Cave = struct
  type t = String.t [@@deriving sexp, equal]

  let start = "start"
  let end_ = "end"
  let is_small (t : t) = Char.is_lowercase (String.get t 0)

  let parser =
    let open Angstrom in
    take_while1 Char.is_alpha
  ;;
end

module Edge = struct
  type t = Cave.t * Cave.t [@@deriving sexp]

  let parser =
    let open Angstrom in
    both (Cave.parser <* char '-') Cave.parser
  ;;
end

module Budget = struct
  type t =
    | Infinite
    | Finite of int

  let is_zero = function
    | Finite 0 -> true
    | Finite _ | Infinite -> false
  ;;

  let pred = function
    | Infinite -> Infinite
    | Finite t -> Finite (pred t)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Edge)
  module Output = Int

  let make_adjacency_lists t =
    let t_reversed = List.map t ~f:Tuple2.swap in
    String.Map.of_alist_multi (t @ t_reversed)
  ;;

  let rec dfs cave budgets adjacency_lists =
    let budget = Map.find_exn budgets cave in
    match Budget.is_zero budget with
    | true -> []
    | false ->
      (match Cave.equal cave Cave.end_ with
      | true -> [ [ Cave.end_ ] ]
      | false ->
        let budgets = Map.set budgets ~key:cave ~data:(Budget.pred budget) in
        let neighbors = Map.find_exn adjacency_lists cave in
        List.concat_map neighbors ~f:(fun neighbor ->
            dfs neighbor budgets adjacency_lists)
        |> List.map ~f:(List.cons cave))
  ;;

  let count_unique_string_lists ls =
    let module String_list_set =
      Set.Make (struct
        type t = string list [@@deriving compare, sexp]
      end)
    in
    String_list_set.of_list ls |> Set.length
  ;;

  let basic_budgets adjacency_lists =
    Map.mapi adjacency_lists ~f:(fun ~key:cave ~data:_ : Budget.t ->
        match Cave.is_small cave with
        | false -> Infinite
        | true -> Finite 1)
  ;;
end

module Part_01 = struct
  include Common

  let solve input =
    let adjacency_lists = make_adjacency_lists input in
    let budgets = basic_budgets adjacency_lists in
    dfs Cave.start budgets adjacency_lists |> count_unique_string_lists
  ;;
end

module Part_02 = struct
  include Common

  let solve input =
    let adjacency_lists = make_adjacency_lists input in
    let inner_small_caves =
      Map.keys adjacency_lists
      |> List.filter ~f:(fun cave ->
             Cave.is_small cave
             && (not (Cave.equal cave Cave.start))
             && not (Cave.equal cave Cave.end_))
    in
    List.concat_map inner_small_caves ~f:(fun special_cave ->
        let budgets =
          Map.set (basic_budgets adjacency_lists) ~key:special_cave ~data:(Finite 2)
        in
        dfs Cave.start budgets adjacency_lists)
    |> count_unique_string_lists
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

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

    let%expect_test _ =
      print_s [%sexp (Part_02.solve test_case : int)];
      [%expect {| 36 |}]
    ;;
  end)
;;
