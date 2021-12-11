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

let parts : (module Solution.Part) list = [ (module Part_01) ]
