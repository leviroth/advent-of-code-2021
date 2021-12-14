open! Core_kernel
open! Import
module Char_pair = Tuple.Comparable (Char) (Char)

module Input' = struct
  type t =
    { polymer_template : char list
    ; insertion_rules : char Char_pair.Map.t
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    let alpha_char = satisfy Char.is_alpha in
    let insertion_rule = both (both alpha_char alpha_char <* string " -> ") alpha_char in
    map2
      (many alpha_char <* end_of_line <* end_of_line)
      (sep_by1 end_of_line insertion_rule >>| Char_pair.Map.of_alist_exn)
      ~f:(fun polymer_template insertion_rules -> { polymer_template; insertion_rules })
  ;;

  let template_string { polymer_template; _ } = String.of_char_list polymer_template
end

module Common = struct
  module Input = Input.Make_parseable (Input')
end

module Part_01 = struct
  include Common
  module Output = Int

  let step ({ polymer_template; insertion_rules } as t : Input.t) =
    let rec aux template acc =
      match template with
      | a :: (b :: _ as tl) ->
        (match Map.find insertion_rules (a, b) with
        | None -> aux tl (a :: acc)
        | Some char -> aux tl (char :: a :: acc))
      | [ a ] -> aux [] (a :: acc)
      | [] -> { t with polymer_template = List.rev acc }
    in
    aux polymer_template []
  ;;

  let solve input =
    let final =
      Sequence.range 0 10 |> Sequence.fold ~init:input ~f:(fun input _ -> step input)
    in
    let counts =
      String.fold
        (Input'.template_string final)
        ~init:Char.Map.empty
        ~f:
          (Map.update ~f:(function
              | None -> 1
              | Some n -> n + 1))
      |> Map.to_alist
    in
    let compare = [%compare: _ * int] in
    snd (Option.value_exn (List.max_elt counts ~compare))
    - snd (Option.value_exn (List.min_elt counts ~compare))
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]

let%test_module _ =
  (module struct
    open Common

    let test_case =
      Input.of_string
        {|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C|}
    ;;

    let%expect_test _ =
      print_s [%sexp (test_case : Input'.t)];
      [%expect
        {|
        ((polymer_template (N N C B))
         (insertion_rules
          (((B B) N) ((B C) B) ((B H) H) ((B N) B) ((C B) H) ((C C) N) ((C H) B)
           ((C N) C) ((H B) C) ((H C) B) ((H H) N) ((H N) C) ((N B) B) ((N C) B)
           ((N H) C) ((N N) C)))) |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Input'.template_string (Part_01.step test_case) : string)];
      [%expect {| NCNBCHB |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : int)];
      [%expect {| 1588 |}]
    ;;
  end)
;;
