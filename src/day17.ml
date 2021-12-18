open! Core_kernel
open! Import

module Target = struct
  type t =
    { x_range : int * int
    ; y_range : int * int
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    let parse_int_positive_or_negative =
      map2
        (option Sign.Pos (char '-' *> return Sign.Neg))
        parse_int
        ~f:(fun sign n -> n * Sign.to_int sign)
    in
    map4
      (string "target area: x=" *> parse_int_positive_or_negative)
      (string ".." *> parse_int_positive_or_negative)
      (string ", y=" *> parse_int_positive_or_negative)
      (string ".." *> parse_int_positive_or_negative)
      ~f:(fun x_low x_high y_low y_high ->
        { x_range = x_low, x_high; y_range = y_low, y_high })
  ;;

  let intervals { x_range = x_low, x_high; y_range = y_low, y_high } =
    ( (Maybe_bound.Incl x_low, Maybe_bound.Incl x_high)
    , (Maybe_bound.Incl y_low, Maybe_bound.Incl y_high) )
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Target)
  module Output = Int

  let step (x, y) (dx, dy) =
    let new_pos = x + dx, y + dy in
    let new_dx =
      match Sign.of_int dx with
      | Neg -> dx + 1
      | Pos -> dx - 1
      | Zero -> 0
    in
    let new_dy = dy - 1 in
    new_pos, (new_dx, new_dy)
  ;;

  let simulate (target : Target.t) velocity =
    let (x_lower, x_upper), (y_lower, y_upper) = Target.intervals target in
    let rec loop pos ~velocity ~max_height =
      let ((new_x, new_y) as new_pos), new_velocity = step pos velocity in
      let max_height = max new_y max_height in
      let x_relative_position =
        Maybe_bound.compare_to_interval_exn new_x ~lower:x_lower ~upper:x_upper ~compare
      in
      let y_relative_position =
        Maybe_bound.compare_to_interval_exn new_y ~lower:y_lower ~upper:y_upper ~compare
      in
      match x_relative_position, y_relative_position with
      | In_range, In_range -> Some max_height
      | _ ->
        let moving_away =
          let x_axis =
            let sign = Sign.of_int (fst new_velocity) in
            match x_relative_position, sign with
            | Below_lower_bound, Neg | Above_upper_bound, Pos -> true
            | In_range, _ | Below_lower_bound, Pos | Above_upper_bound, Neg | _, Zero ->
              false
          in
          let y_axis =
            let sign = Sign.of_int (snd new_velocity) in
            match y_relative_position, sign with
            | Below_lower_bound, Neg -> true
            | Above_upper_bound, Pos
            | In_range, _
            | Below_lower_bound, Pos
            | Above_upper_bound, Neg
            | _, Zero -> false
          in
          x_axis || y_axis
        in
        (match moving_away with
        | true -> None
        | false -> loop new_pos ~velocity:new_velocity ~max_height)
    in
    loop (0, 0) ~velocity ~max_height:0
  ;;

  let all_max_heights (input : Target.t) =
    Sequence.range ~stop:`inclusive 0 (snd input.x_range)
    |> Sequence.concat_map ~f:(fun dx ->
           Sequence.range (-1000) 1000
           |> Sequence.filter_map ~f:(fun dy -> simulate input (dx, dy)))
  ;;
end

module Part_01 = struct
  include Common

  let solve input = all_max_heights input |> Sequence.max_elt ~compare |> Option.value_exn
end

module Part_02 = struct
  include Common

  let solve input = all_max_heights input |> Sequence.length
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

let%test_module _ =
  (module struct
    open Common

    let test_case = Input.of_string "target area: x=20..30, y=-10..-5"

    let%expect_test _ =
      let test velocity =
        print_s [%sexp (Part_01.simulate test_case velocity : int option)]
      in
      test (7, 2);
      [%expect {| (3) |}];
      test (6, 3);
      [%expect {| (6) |}];
      test (9, 0);
      [%expect {| (0) |}];
      test (17, -4);
      [%expect {| () |}];
      test (6, 9);
      [%expect {| (45) |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : int)];
      [%expect {| 45 |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_02.solve test_case : int)];
      [%expect {| 112 |}]
    ;;
  end)
;;
