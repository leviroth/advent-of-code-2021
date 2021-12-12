open! Core_kernel
open! Import

module One_input = struct
  type t =
    { signal_patterns : string list
    ; outputs : string list
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    let signal = take_while1 Char.is_alpha in
    let repeated = sep_by1 (char ' ') signal in
    map2
      (repeated <* string " | ")
      repeated
      ~f:(fun signal_patterns outputs -> { signal_patterns; outputs })
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (One_input)
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve =
    let magic_numbers = Int.Set.of_list [ 2; 4; 3; 7 ] in
    List.sum
      (module Int)
      ~f:(fun ({ outputs; signal_patterns = _ } : One_input.t) ->
        List.count outputs ~f:(fun output -> Set.mem magic_numbers (String.length output)))
  ;;

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|}
    in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 26 |}]
  ;;
end

module Part_02 = struct
  include Common

  let all_digits =
    String.Map.of_alist_exn
      [ "abcefg", 0
      ; "cf", 1
      ; "acdeg", 2
      ; "acdfg", 3
      ; "bcdf", 4
      ; "abdfg", 5
      ; "abdefg", 6
      ; "acf", 7
      ; "abcdefg", 8
      ; "abcdfg", 9
      ]
  ;;

  let apply_scramble_and_sort signal ~scramble =
    String.map signal ~f:(fun char ->
        let index = Char.to_int char - Char.to_int 'a' in
        String.get scramble index)
    |> String.to_list
    |> List.sort ~compare:Char.compare
    |> String.of_char_list
  ;;

  let compatible signal ~scramble =
    apply_scramble_and_sort signal ~scramble |> Map.mem all_digits
  ;;

  let%expect_test _ =
    let test signal scramble =
      print_s
        [%message
          ""
            ~apply_scramble_and_sort:(apply_scramble_and_sort signal ~scramble : string)
            ~compatible:(compatible signal ~scramble : bool)]
    in
    test "be" "bcegfad";
    [%expect {| ((apply_scramble_and_sort cf) (compatible true)) |}];
    test "be" "bfegcad";
    [%expect {| ((apply_scramble_and_sort cf) (compatible true)) |}]
  ;;

  let permutations s =
    let drop_index l ~index = List.filteri l ~f:(fun i _ -> i <> index) in
    let rec permutations l =
      match l with
      | [] -> []
      | [ x ] -> [ [ x ] ]
      | _ ->
        List.concat_mapi l ~f:(fun index c ->
            let sub_permutations = permutations (drop_index l ~index) in
            List.map sub_permutations ~f:(fun permutation -> c :: permutation))
    in
    List.map (permutations (String.to_list s)) ~f:String.of_char_list
  ;;

  let%expect_test "permutations" =
    print_s [%sexp (permutations "abc" : string list)];
    [%expect {| (abc acb bac bca cab cba) |}]
  ;;

  let all_scrambles = lazy (permutations "abcdefg")

  let solve_one ({ signal_patterns; outputs } : One_input.t) =
    match
      List.fold
        (signal_patterns @ outputs)
        ~init:(Lazy.force all_scrambles)
        ~f:(fun scrambles signal ->
          List.filter scrambles ~f:(fun scramble -> compatible signal ~scramble))
    with
    | [ scramble ] ->
      List.fold outputs ~init:0 ~f:(fun sum output ->
          let digit =
            Map.find_exn all_digits (apply_scramble_and_sort output ~scramble)
          in
          (10 * sum) + digit)
    | [] | _ :: _ -> assert false
  ;;

  let solve = List.sum (module Int) ~f:solve_one

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|}
    in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 61229 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
