open! Core_kernel
open! Import

module Common = struct
  module Input = Input.String_list
  module Output = Int

  let test_case =
    Input.of_string
      {|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|}
  ;;

  let int_of_bit_string bit_string =
    String.fold bit_string ~init:0 ~f:(fun acc c ->
        (acc lsl 1)
        +
        match c with
        | '0' -> 0
        | '1' -> 1
        | _ -> assert false)
  ;;

  let%expect_test _ =
    let test s = printf "%d\n" (int_of_bit_string s) in
    test "0";
    [%expect {| 0 |}];
    test "1";
    [%expect {| 1 |}];
    test "10";
    [%expect {| 2 |}];
    test "011";
    [%expect {| 3 |}];
    test "1011";
    [%expect {| 11 |}]
  ;;
end

module Part_01 = struct
  include Common

  let solve numbers =
    let string_length = String.length (List.hd_exn numbers) in
    let list_length = List.length numbers in
    let gamma_for_positon position =
      let zeros =
        List.count numbers ~f:(fun s -> Char.equal '0' (String.get s position))
      in
      let ones = list_length - zeros in
      match zeros > ones with
      | true -> 0
      | false -> 1
    in
    let gamma =
      List.range 0 string_length
      |> List.fold ~init:0 ~f:(fun acc position ->
             (acc lsl 1) + gamma_for_positon position)
    in
    let epsilon =
      let mask =
        List.range 0 string_length |> List.fold ~init:0 ~f:(fun acc _ -> (acc lsl 1) + 1)
      in
      lnot gamma land mask
    in
    gamma * epsilon
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 198 |}]
  ;;
end

module Part_02 = struct
  include Common

  let filter_to_one_elt l ~filter =
    let rec aux l ~position =
      match filter l ~position with
      | [ x ] -> x
      | [] -> assert false
      | _ :: _ as l -> aux l ~position:(position + 1)
    in
    aux l ~position:0
  ;;

  let filter l ~position ~rule =
    let bits = List.map l ~f:(fun s -> String.get s position) in
    let rule = unstage (rule bits) in
    List.filter l ~f:(fun s -> rule (String.get s position))
  ;;

  let oxygen_generator_rule bits =
    let ones = List.count bits ~f:(Char.equal '1') in
    let zeros = List.count bits ~f:(Char.equal '0') in
    stage
      (match ones >= zeros with
      | true -> Char.equal '1'
      | false -> Char.equal '0')
  ;;

  let co2_scrubber_rule bits =
    let ones = List.count bits ~f:(Char.equal '1') in
    let zeros = List.count bits ~f:(Char.equal '0') in
    stage
      (match zeros <= ones with
      | true -> Char.equal '0'
      | false -> Char.equal '1')
  ;;

  let solve numbers =
    let oxygen_generator =
      filter_to_one_elt numbers ~filter:(filter ~rule:oxygen_generator_rule)
    in
    let co2_scrubber =
      filter_to_one_elt numbers ~filter:(filter ~rule:co2_scrubber_rule)
    in
    int_of_bit_string oxygen_generator * int_of_bit_string co2_scrubber
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 230 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
