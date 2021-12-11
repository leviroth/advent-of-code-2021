open! Core_kernel
open! Import

module Input = Input.Make_parseable (struct
  type t = int list

  let parser = Angstrom.(sep_by (char ',') parse_int)
end)

module Common = struct
  module Input = Input
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve input =
    let counters =
      List.fold
        input
        ~init:Int.Map.empty
        ~f:
          (Map.update ~f:(function
              | None -> 1
              | Some n -> n + 1))
    in
    List.range 0 80
    |> List.fold ~init:counters ~f:(fun counters _ ->
           Map.to_alist counters
           |> List.concat_map ~f:(fun (days, num_fish) ->
                  match days with
                  | 0 -> [ 6, num_fish; 8, num_fish ]
                  | _ -> [ days - 1, num_fish ])
           |> Int.Map.of_alist_reduce ~f:( + ))
    |> Map.data
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let test_case = Input.of_string "3,4,3,1,2" in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 5934 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
