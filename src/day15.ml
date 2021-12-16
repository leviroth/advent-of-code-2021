open! Core_kernel
open! Import

module Grid = struct
  type t = int array array [@@deriving sexp]

  let parser =
    let open Angstrom in
    let one =
      many1 (satisfy Char.is_digit)
      |> map ~f:(fun l ->
             List.map l ~f:(fun c -> Char.to_int c - Char.to_int '0') |> Array.of_list)
    in
    sep_by1 end_of_line one |> map ~f:Array.of_list
  ;;

  let print t =
    Array.iter t ~f:(fun array ->
        Array.iter array ~f:(printf "%d");
        printf "\n")
  ;;

  let project_value value ~repetitions = ((value - 1 + repetitions) % 9) + 1

  let get t (row, col) =
    let rows = Array.length t in
    let cols = Array.length t.(0) in
    let row_repetitions = row / rows in
    let col_repetitions = col / cols in
    let original_row_index = row % rows in
    let original_col_index = col % cols in
    t.(original_row_index).(original_col_index)
    |> project_value ~repetitions:col_repetitions
    |> project_value ~repetitions:row_repetitions
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Grid)
  module Output = Int
  module Heap = Hash_heap.Make (Int_pair)

  let solve input ~repetitions =
    let rows = Array.length input * repetitions in
    let cols = Array.length input.(0) * repetitions in
    let compare_distance a b =
      match a, b with
      | None, None -> 0
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some a, Some b -> compare a b
    in
    let heap = Heap.create ~min_size:(rows * cols) compare_distance in
    Sequence.cartesian_product (Sequence.range 0 rows) (Sequence.range 0 cols)
    |> Sequence.iter ~f:(fun coords -> Heap.push_exn heap ~key:coords ~data:None);
    Heap.replace heap ~key:(0, 0) ~data:(Some 0);
    Sequence.fold_until
      (Sequence.repeat ())
      ~init:()
      ~f:(fun () () ->
        let coords, risk = Heap.pop_with_key_exn heap in
        let risk = Option.value_exn risk in
        match Int_pair.equal coords (rows - 1, cols - 1) with
        | true -> Continue_or_stop.Stop risk
        | false ->
          let neighbor_coords =
            Int_pair.neighbors coords Int_pair.right_vectors
            |> List.filter ~f:(Heap.mem heap)
          in
          List.iter neighbor_coords ~f:(fun neighbor_coords ->
              let new_risk =
                Base.Comparable.min
                  compare_distance
                  (Heap.find_exn heap neighbor_coords)
                  (Some (risk + Grid.get input neighbor_coords))
              in
              Heap.replace heap ~key:neighbor_coords ~data:new_risk);
          Continue ())
      ~finish:(fun () -> assert false)
  ;;

  (* This solution happens to work for part 1, but it assumes that we only move
     down or to the right. *)
  let solve' input ~repetitions =
    let rows = Array.length input * repetitions in
    let cols = Array.length input.(0) * repetitions in
    let risks = Array.init rows ~f:(fun _ -> Array.init cols ~f:(const None)) in
    let get_risk (row, col) =
      let get_opt array index = Option.try_with (fun () -> Array.get array index) in
      let open Option.Let_syntax in
      let%bind row = get_opt risks row in
      let%bind risk = get_opt row col in
      risk
    in
    Sequence.cartesian_product (Sequence.range 0 rows) (Sequence.range 0 cols)
    |> Sequence.iter ~f:(fun (row, col) ->
           risks.(row).(col)
             <- Some
                  (match
                     List.filter_map [ row - 1, col; row, col - 1 ] ~f:get_risk
                     |> List.min_elt ~compare
                   with
                  | None -> 0
                  | Some v -> v + Grid.get input (row, col)));
    Option.value_exn risks.(rows - 1).(cols - 1)
  ;;
end

module Part_01 = struct
  include Common

  let solve = solve ~repetitions:1
end

module Part_02 = struct
  include Common

  let solve = solve ~repetitions:5
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

let%test_module _ =
  (module struct
    open Common

    let test_case =
      Input.of_string
        {|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581|}
    ;;

    let%expect_test _ =
      print_s [%sexp (test_case : Grid.t)];
      [%expect
        {|
        ((1 1 6 3 7 5 1 7 4 2) (1 3 8 1 3 7 3 6 7 2) (2 1 3 6 5 1 1 3 2 8)
         (3 6 9 4 9 3 1 5 6 9) (7 4 6 3 4 1 7 1 1 1) (1 3 1 9 1 2 8 1 3 7)
         (1 3 5 9 9 1 2 4 2 1) (3 1 2 5 4 2 1 6 3 9) (1 2 9 3 1 3 8 5 2 1)
         (2 3 1 1 9 4 4 5 8 1)) |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : Output.t)];
      [%expect {| 40 |}]
    ;;

    let%expect_test _ =
      let test value repetitions =
        let result = Grid.project_value value ~repetitions in
        print_s [%message "" (value : int) (repetitions : int) (result : int)]
      in
      test 1 1;
      test 9 1;
      test 8 4;
      [%expect
        {|
        ((value 1) (repetitions 1) (result 2))
        ((value 9) (repetitions 1) (result 1))
        ((value 8) (repetitions 4) (result 3)) |}];
      let rows = Array.length test_case * 5 in
      let cols = Array.length test_case.(0) * 5 in
      let expanded_grid =
        Array.init rows ~f:(fun row ->
            Array.init cols ~f:(fun col -> Grid.get test_case (row, col)))
      in
      Grid.print expanded_grid;
      [%expect {|
        11637517422274862853338597396444961841755517295286
        13813736722492484783351359589446246169155735727126
        21365113283247622439435873354154698446526571955763
        36949315694715142671582625378269373648937148475914
        74634171118574528222968563933317967414442817852555
        13191281372421239248353234135946434524615754563572
        13599124212461123532357223464346833457545794456865
        31254216394236532741534764385264587549637569865174
        12931385212314249632342535174345364628545647573965
        23119445813422155692453326671356443778246755488935
        22748628533385973964449618417555172952866628316397
        24924847833513595894462461691557357271266846838237
        32476224394358733541546984465265719557637682166874
        47151426715826253782693736489371484759148259586125
        85745282229685639333179674144428178525553928963666
        24212392483532341359464345246157545635726865674683
        24611235323572234643468334575457944568656815567976
        42365327415347643852645875496375698651748671976285
        23142496323425351743453646285456475739656758684176
        34221556924533266713564437782467554889357866599146
        33859739644496184175551729528666283163977739427418
        35135958944624616915573572712668468382377957949348
        43587335415469844652657195576376821668748793277985
        58262537826937364893714847591482595861259361697236
        96856393331796741444281785255539289636664139174777
        35323413594643452461575456357268656746837976785794
        35722346434683345754579445686568155679767926678187
        53476438526458754963756986517486719762859782187396
        34253517434536462854564757396567586841767869795287
        45332667135644377824675548893578665991468977611257
        44961841755517295286662831639777394274188841538529
        46246169155735727126684683823779579493488168151459
        54698446526571955763768216687487932779859814388196
        69373648937148475914825958612593616972361472718347
        17967414442817852555392896366641391747775241285888
        46434524615754563572686567468379767857948187896815
        46833457545794456865681556797679266781878137789298
        64587549637569865174867197628597821873961893298417
        45364628545647573965675868417678697952878971816398
        56443778246755488935786659914689776112579188722368
        55172952866628316397773942741888415385299952649631
        57357271266846838237795794934881681514599279262561
        65719557637682166874879327798598143881961925499217
        71484759148259586125936169723614727183472583829458
        28178525553928963666413917477752412858886352396999
        57545635726865674683797678579481878968159298917926
        57944568656815567976792667818781377892989248891319
        75698651748671976285978218739618932984172914319528
        56475739656758684176786979528789718163989182927419
        67554889357866599146897761125791887223681299833479 |}]
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_02.solve test_case : Output.t)];
      [%expect {| 315 |}]
    ;;
  end)
;;
