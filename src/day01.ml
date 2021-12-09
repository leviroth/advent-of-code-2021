open! Core_kernel
open! Import

module Common = struct
  module Input = Input.Int_list
  module Output = Int

  let test_case = Input.of_string {|199
200
208
210
200
207
240
269
260
263|}

  let sliding_windows l lengths =
    let last_window, windows =
      List.fold_map l ~init:Fqueue.empty ~f:(fun queue v ->
          match equal lengths (Fqueue.length queue) with
          | false -> Fqueue.enqueue queue v, None
          | true ->
            let window = Fqueue.to_list queue in
            let queue = Fqueue.drop_exn queue in
            let queue = Fqueue.enqueue queue v in
            queue, Some window)
    in
    List.filter_opt windows @ [ Fqueue.to_list last_window ]
  ;;
end

module Part_01 = struct
  include Common

  let solve numbers =
    sliding_windows numbers 2
    |> List.count ~f:(function
           | a :: b :: _ -> a < b
           | _ -> assert false)
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 7 |}]
  ;;
end

module Part_02 = struct
  include Common

  let solve numbers =
    sliding_windows numbers 3
    |> List.map ~f:(List.sum (module Int) ~f:Fn.id)
    |> Part_01.solve
  ;;

  let%expect_test _ =
    print_s [%sexp (solve test_case : int)];
    [%expect {| 5 |}]
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
