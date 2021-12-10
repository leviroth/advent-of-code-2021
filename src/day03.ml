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
    let gamma_for_position position =
      let zeros =
        List.count numbers ~f:(fun s -> Char.equal '0' (String.get s position))
      in
      let ones = list_length - zeros in
      match zeros > ones with
      | true -> '0'
      | false -> '1'
    in
    let gamma =
      List.range 0 string_length
      |> List.map ~f:gamma_for_position
      |> String.of_char_list
      |> int_of_bit_string
    in
    let epsilon =
      let mask = (1 lsl string_length) - 1 in
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

  let rule_of_counts bits ~expected_char =
    let ones = List.count bits ~f:(Char.equal '1') in
    let zeros = List.count bits ~f:(Char.equal '0') in
    let expected_char = expected_char ~ones ~zeros in
    stage (Char.equal expected_char)
  ;;

  let oxygen_generator_rule =
    rule_of_counts ~expected_char:(fun ~ones ~zeros ->
        match ones >= zeros with
        | true -> '1'
        | false -> '0')
  ;;

  let co2_scrubber_rule =
    rule_of_counts ~expected_char:(fun ~ones ~zeros ->
        match zeros <= ones with
        | true -> '0'
        | false -> '1')
  ;;

  let solve numbers =
    List.fold [ oxygen_generator_rule; co2_scrubber_rule ] ~init:1 ~f:(fun acc rule ->
        acc * int_of_bit_string (filter_to_one_elt numbers ~filter:(filter ~rule)))
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 230 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
