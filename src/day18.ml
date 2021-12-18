open! Core_kernel
open! Import

module Number = struct
  type t =
    | Regular of int
    | Pair of t * t
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    fix (fun parser ->
        choice
          [ parse_int |> map ~f:(fun n -> Regular n)
          ; char '['
            *> map2 (parser <* char ',') (parser <* char ']') ~f:(fun a b -> Pair (a, b))
          ])
  ;;

  let rec to_string t =
    match t with
    | Regular n -> Int.to_string n
    | Pair (l, r) -> sprintf "[%s,%s]" (to_string l) (to_string r)
  ;;

  let add' l r = Pair (l, r)

  let rec add_value_on_left_side t value =
    match t with
    | Regular n -> Regular (n + value)
    | Pair (left, right) -> Pair (add_value_on_left_side left value, right)
  ;;

  let rec add_value_on_right_side t value =
    match t with
    | Regular n -> Regular (n + value)
    | Pair (left, right) -> Pair (left, add_value_on_right_side right value)
  ;;

  let handle_explosion_left (new_t_left, left_value, right_value) right_t =
    let new_t_left =
      let new_t_right =
        match right_value with
        | None -> right_t
        | Some v -> add_value_on_left_side right_t v
      in
      Pair (new_t_left, new_t_right)
    in
    new_t_left, left_value, None
  ;;

  let handle_explosion_right left_t (new_t_right, left_value, right_value) =
    let new_t_right =
      let new_t_left =
        match left_value with
        | None -> left_t
        | Some v -> add_value_on_right_side left_t v
      in
      Pair (new_t_left, new_t_right)
    in
    new_t_right, None, right_value
  ;;

  let find_and_apply_explosion t =
    let rec aux t depth =
      match t with
      | Regular _ -> `No_change
      | Pair (Regular l, Regular r) ->
        (match depth >= 4 with
        | false -> `No_change
        | true -> `Explosion (Regular 0, Some l, Some r))
      | Pair (l, r) ->
        let depth = depth + 1 in
        (match aux l depth with
        | `Explosion v -> `Explosion (handle_explosion_left v r)
        | `No_change ->
          (match aux r depth with
          | `Explosion v -> `Explosion (handle_explosion_right l v)
          | `No_change -> `No_change))
    in
    match aux t 0 with
    | `No_change -> `No_change
    | `Explosion (new_t, _, _) -> `Exploded new_t
  ;;

  let split n =
    let rounded_down = n / 2 in
    let rounded_up = rounded_down + (n % 2) in
    Pair (Regular rounded_down, Regular rounded_up)
  ;;

  let find_and_split t =
    let rec aux t =
      match t with
      | Regular n ->
        (match n >= 10 with
        | false -> `No_change
        | true -> `Split (split n))
      | Pair (l, r) ->
        (match aux l with
        | `Split new_l -> `Split (Pair (new_l, r))
        | `No_change ->
          (match aux r with
          | `Split new_r -> `Split (Pair (l, new_r))
          | `No_change -> `No_change))
    in
    aux t
  ;;

  let reduce t =
    let rec aux t =
      match find_and_apply_explosion t with
      | `Exploded t -> aux t
      | `No_change ->
        (match find_and_split t with
        | `Split t -> aux t
        | `No_change -> t)
    in
    aux t
  ;;

  let add a b = reduce (add' a b)

  let rec magnitude t =
    match t with
    | Regular n -> n
    | Pair (l, r) -> (3 * magnitude l) + (2 * magnitude r)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Number)
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve numbers = Number.magnitude (List.reduce_exn numbers ~f:Number.add)
end

module Part_02 = struct
  include Common

  let all_pairs input =
    let input_sequence = Sequence.of_list input in
    let indexed = Sequence.mapi input_sequence ~f:Tuple2.create in
    Sequence.cartesian_product indexed indexed
    |> Sequence.filter_map ~f:(fun ((i, a), (i', b)) ->
           match i = i' with
           | true -> None
           | false -> Some (a, b))
  ;;

  let solve input =
    Sequence.map (all_pairs input) ~f:(fun (a, b) -> Number.magnitude (Number.add a b))
    |> Sequence.max_elt ~compare
    |> Option.value_exn
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

let%test_module _ =
  (module struct
    open Common

    let test_case =
      Input.of_string
        {|[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]|}
    ;;

    let%expect_test "parsing" =
      print_s [%sexp (test_case : Number.t list)];
      [%expect
        {|
        ((Pair (Regular 1) (Regular 2))
         (Pair (Pair (Regular 1) (Regular 2)) (Regular 3))
         (Pair (Regular 9) (Pair (Regular 8) (Regular 7)))
         (Pair (Pair (Regular 1) (Regular 9)) (Pair (Regular 8) (Regular 5)))
         (Pair
          (Pair (Pair (Pair (Regular 1) (Regular 2)) (Pair (Regular 3) (Regular 4)))
           (Pair (Pair (Regular 5) (Regular 6)) (Pair (Regular 7) (Regular 8))))
          (Regular 9))
         (Pair
          (Pair (Pair (Regular 9) (Pair (Regular 3) (Regular 8)))
           (Pair (Pair (Regular 0) (Regular 9)) (Regular 6)))
          (Pair (Pair (Pair (Regular 3) (Regular 7)) (Pair (Regular 4) (Regular 9)))
           (Regular 3)))
         (Pair
          (Pair (Pair (Pair (Regular 1) (Regular 3)) (Pair (Regular 5) (Regular 3)))
           (Pair (Pair (Regular 1) (Regular 3)) (Pair (Regular 8) (Regular 7))))
          (Pair (Pair (Pair (Regular 4) (Regular 9)) (Pair (Regular 6) (Regular 9)))
           (Pair (Pair (Regular 8) (Regular 2)) (Pair (Regular 7) (Regular 3)))))) |}]
    ;;

    let%expect_test "explosions" =
      let test input =
        let test_case = List.hd_exn (Input.of_string input) in
        let exploded =
          match Number.find_and_apply_explosion test_case with
          | `No_change -> assert false
          | `Exploded v -> v
        in
        printf "%s\n" (Number.to_string exploded)
      in
      test "[[[[[9,8],1],2],3],4]";
      [%expect {| [[[[0,9],2],3],4] |}];
      test "[7,[6,[5,[4,[3,2]]]]]";
      [%expect {| [7,[6,[5,[7,0]]]] |}];
      test "[[6,[5,[4,[3,2]]]],1]";
      [%expect {| [[6,[5,[7,0]]],3] |}];
      test "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]";
      [%expect {| [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] |}];
      test "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]";
      [%expect {| [[3,[2,[8,0]]],[9,[5,[7,0]]]] |}]
    ;;

    let%expect_test "addition" =
      let parse_one_input s = List.hd_exn (Input.of_string s) in
      let test_case_a = parse_one_input "[[[[4,3],4],4],[7,[[8,4],9]]]" in
      let test_case_b = parse_one_input "[1,1]" in
      printf "%s\n" Number.(to_string (reduce (add test_case_a test_case_b)));
      [%expect {| [[[[0,7],4],[[7,8],[6,0]]],[8,1]] |}]
    ;;

    let test_case =
      Input.of_string
        {|[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]|}
    ;;

    let%expect_test "Part 1" =
      print_s [%sexp (Part_01.solve test_case : Part_01.Output.t)];
      [%expect {| 4140 |}]
    ;;

    let%expect_test "Part 2" =
      print_s [%sexp (Part_02.solve test_case : Part_02.Output.t)];
      [%expect {| 3993 |}]
    ;;
  end)
;;
