open! Core_kernel
open! Import

module Common = struct
  module Input = Input.Make_parseable_many (struct
    type t = string

    let parser = Angstrom.take_while1 (fun c -> not (Char.equal c '\n'))
  end)

  module Output = Int
end

module Part_01 = struct
  include Common

  let char_info = [ '(', ')', 3; '[', ']', 57; '{', '}', 1197; '<', '>', 25137 ]

  let opening_by_closing =
    List.map char_info ~f:(fun (opening, closing, _score) -> closing, opening)
    |> Char.Map.of_alist_exn
  ;;

  let score_by_closing =
    List.map char_info ~f:(fun (_opening, closing, score) -> closing, score)
    |> Char.Map.of_alist_exn
  ;;

  let find_illegal_character line =
    List.fold_until
      (String.to_list line)
      ~init:[]
      ~f:(fun stack c ->
        match Map.find opening_by_closing c with
        | None -> Continue (c :: stack)
        | Some opening ->
          (match stack with
          | [] -> Stop (Some c)
          | hd :: tl ->
            (match Char.equal hd opening with
            | false -> Stop (Some c)
            | true -> Continue tl)))
      ~finish:(fun _ -> None)
  ;;

  let solve input =
    List.filter_map input ~f:find_illegal_character
    |> List.map ~f:(Map.find_exn score_by_closing)
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let test_case =
      Input.of_string
        {|[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]|}
    in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 26397 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
