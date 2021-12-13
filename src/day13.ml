open! Core_kernel
open! Import

module Dot = struct
  include Int_pair

  let parser =
    let open Angstrom in
    both (parse_int <* char ',') parse_int
  ;;
end

module Axis = struct
  type t =
    | X
    | Y
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    map any_char ~f:(function
        | 'x' -> X
        | 'y' -> Y
        | _ -> assert false)
  ;;
end

module Fold_instruction = struct
  type t =
    { axis : Axis.t
    ; position : int
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    map2
      (string "fold along " *> Axis.parser <* char '=')
      parse_int
      ~f:(fun axis position -> { axis; position })
  ;;

  let apply { position; axis } ~dots =
    let flip_dimension v = (2 * position) - v in
    let will_move =
      match axis with
      | X -> fun (x, _) -> x > position
      | Y -> fun (_, y) -> y > position
    in
    let move_dot =
      match axis with
      | X -> fun (x, y) -> flip_dimension x, y
      | Y -> fun (x, y) -> x, flip_dimension y
    in
    Int_pair.Set.map dots ~f:(fun dot ->
        match will_move dot with
        | false -> dot
        | true -> move_dot dot)
  ;;
end

module Input' = struct
  type t =
    { dots : Dot.Set.t
    ; fold_instructions : Fold_instruction.t list
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    map2
      (sep_by1 end_of_line Dot.parser <* end_of_line <* end_of_line)
      (sep_by1 end_of_line Fold_instruction.parser)
      ~f:(fun dots fold_instructions ->
        { dots = Dot.Set.of_list dots; fold_instructions })
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Input')
end

module Part_01 = struct
  include Common
  module Output = Int

  let solve ({ dots; fold_instructions } : Input.t) =
    let fold = List.hd_exn fold_instructions in
    Set.length (Fold_instruction.apply fold ~dots)
  ;;
end

module Part_02 = struct
  include Common

  module Output = struct
    type t = Dot.Set.t

    let to_string t = Dot.set_to_string t
  end

  let solve ({ dots; fold_instructions } : Input.t) =
    List.fold fold_instructions ~init:dots ~f:(fun dots instruction ->
        Fold_instruction.apply instruction ~dots)
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

let%test_module _ =
  (module struct
    open Common

    let test_case =
      Input.of_string
        {|6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5|}
    ;;

    let%expect_test _ =
      print_s [%sexp (test_case : Input'.t)];
      [%expect
        {|
        ((dots
          ((0 3) (0 13) (0 14) (1 10) (2 14) (3 0) (3 4) (4 1) (4 11) (6 0) (6 10)
           (6 12) (8 4) (8 10) (9 0) (9 10) (10 4) (10 12)))
         (fold_instructions (((axis Y) (position 7)) ((axis X) (position 5))))) |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : int)];
      [%expect {| 17 |}]
    ;;

    let%expect_test _ =
      let solution = Part_02.solve test_case in
      printf "%s\n" (Part_02.Output.to_string solution);
      [%expect
        {|
        #####
        #   #
        #   #
        #   #
        ##### |}]
    ;;
  end)
;;
