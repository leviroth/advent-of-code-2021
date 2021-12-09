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

let parts : (module Solution.Part) list = [ (module Part_01) ]
