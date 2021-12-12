open! Core_kernel
open! Import

module Char_info = struct
  type t =
    { opening : char
    ; closing : char
    ; opening_score : int
    ; closing_score : int
    }

  let all =
    [ { opening = '('; closing = ')'; closing_score = 3; opening_score = 1 }
    ; { opening = '['; closing = ']'; closing_score = 57; opening_score = 2 }
    ; { opening = '{'; closing = '}'; closing_score = 1197; opening_score = 3 }
    ; { opening = '<'; closing = '>'; closing_score = 25137; opening_score = 4 }
    ]
  ;;

  let by_opening =
    Char.Map.of_alist_exn (List.map all ~f:(fun ({ opening; _ } as t) -> opening, t))
  ;;

  let by_closing =
    Char.Map.of_alist_exn (List.map all ~f:(fun ({ closing; _ } as t) -> closing, t))
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (struct
    type t = string

    let parser = Angstrom.take_while1 (fun c -> not (Char.equal c '\n'))
  end)

  module Output = Int

  let classify line =
    List.fold_until
      (String.to_list line)
      ~init:[]
      ~f:(fun stack c ->
        match Map.find Char_info.by_closing c with
        | None -> Continue (c :: stack)
        | Some { opening; _ } ->
          (match stack with
          | [] -> Stop (`Corrupt c)
          | hd :: tl ->
            (match Char.equal hd opening with
            | false -> Stop (`Corrupt c)
            | true -> Continue tl)))
      ~finish:(fun stack -> `Incomplete stack)
  ;;
end

module Part_01 = struct
  include Common

  let solve input =
    List.filter_map input ~f:(fun line ->
        match classify line with
        | `Incomplete _ -> None
        | `Corrupt c -> Some c)
    |> List.map ~f:(fun closing ->
           (Map.find_exn Char_info.by_closing closing).closing_score)
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

module Part_02 = struct
  include Common

  let median l =
    let length = List.length l in
    let index = length / 2 in
    List.nth_exn (List.sort l ~compare) index
  ;;

  let solve input =
    let stacks =
      List.filter_map input ~f:(fun line ->
          match classify line with
          | `Incomplete stack -> Some stack
          | `Corrupt _ -> None)
    in
    List.map stacks ~f:(fun stack ->
        List.fold stack ~init:0 ~f:(fun score c ->
            (score * 5) + (Map.find_exn Char_info.by_opening c).opening_score))
    |> median
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
    [%expect {| 288957 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
