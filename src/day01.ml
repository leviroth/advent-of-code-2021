open! Core_kernel
open! Import

module Common = struct
  module Input = Input.Int_list
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve numbers =
    List.fold numbers ~init:(0, None) ~f:(fun (sum, prev) (current : int) ->
        match prev with
        | None -> sum, Some current
        | Some prev ->
          (match prev < current with
          | true -> sum + 1, Some current
          | false -> sum, Some current))
    |> fst
  ;;

  let%expect_test _ =
    let test_case = Input.of_string {|199
200
208
210
200
207
240
269
260
263|} in
    print_s [%sexp (solve test_case : int)];
    [%expect {| 7 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
