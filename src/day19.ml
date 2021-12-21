open! Core_kernel
open! Import

module Int_triple = struct
  module T = struct
    type t = int * int * int [@@deriving hash, sexp, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let zero = 0, 0, 0
  let add (a_1, b_1, c_1) (a_2, b_2, c_2) = a_1 + a_2, b_1 + b_2, c_1 + c_2
  let sub (a_1, b_1, c_1) (a_2, b_2, c_2) = a_1 - a_2, b_1 - b_2, c_1 - c_2

  let manhattan_distance (a_1, b_1, c_1) (a_2, b_2, c_2) =
    abs (a_1 - a_2) + abs (b_1 - b_2) + abs (c_1 - c_2)
  ;;
end

module Rotation : sig
  type t

  val apply : t -> Int_triple.t -> Int_triple.t
  val all : t list
end = struct
  type t = Int_triple.t * Int_triple.t * Int_triple.t [@@deriving compare]

  let apply
      ((x_of_x, y_of_x, z_of_x), (x_of_y, y_of_y, z_of_y), (x_of_z, y_of_z, z_of_z))
      (x, y, z)
    =
    ( (x * x_of_x) + (y * x_of_y) + (z * x_of_z)
    , (x * y_of_x) + (y * y_of_y) + (z * y_of_z)
    , (x * z_of_x) + (y * z_of_y) + (z * z_of_z) )
  ;;

  let rotate_about_x = (1, 0, 0), (0, 0, 1), (0, -1, 0)
  let rotate_about_y = (0, 0, 1), (0, 1, 0), (-1, 0, 0)
  let rotate_about_z = (0, 1, 0), (-1, 0, 0), (0, 0, 1)

  let all =
    let basis = (1, 0, 0), (0, 1, 0), (0, 0, 1) in
    let transformed_bases =
      let open List.Let_syntax in
      let range = List.range 0 4 in
      let%bind rotate_x =
        let%bind n = range in
        [ Fn.apply_n_times ~n (apply rotate_about_x) ]
      in
      let%bind rotate_y =
        let%bind n = range in
        [ Fn.apply_n_times ~n (apply rotate_about_y) ]
      in
      let%bind rotate_z =
        let%bind n = range in
        [ Fn.apply_n_times ~n (apply rotate_about_z) ]
      in
      [ Tuple3.map basis ~f:(fun v -> rotate_x (rotate_y (rotate_z v))) ]
    in
    List.dedup_and_sort transformed_bases ~compare
  ;;

  let%expect_test _ =
    print_s [%sexp (List.length all : int)];
    [%expect {| 24 |}]
  ;;
end

module Transformation : sig
  type t

  val create : rotation:Rotation.t -> shift:Int_triple.t -> t
  val apply : t -> Int_triple.t -> Int_triple.t
  val zero : t
  val combine : t -> t -> t
end = struct
  module Single = struct
    type t =
      { rotation : Rotation.t
      ; shift : Int_triple.t
      }

    let apply { rotation; shift } coords =
      Rotation.apply rotation coords |> Int_triple.add shift
    ;;
  end

  type t = Single.t list

  let create ~rotation ~shift : t = [ { rotation; shift } ]

  let apply ts coords =
    List.fold ts ~init:coords ~f:(fun coords transformation ->
        Single.apply transformation coords)
  ;;

  let zero = []
  let combine = ( @ )
end

let possible_shifts as_ bs =
  List.cartesian_product as_ bs |> List.map ~f:(fun (a, b) -> Int_triple.sub a b)
;;

let count_overlap a b = Set.length (Set.inter a b)

let find_transformation a b =
  let a_set = Int_triple.Set.of_list a in
  let transformations =
    let open List.Let_syntax in
    let%bind rotation = Rotation.all in
    let rotated = List.map b ~f:(Rotation.apply rotation) in
    let%bind shift = possible_shifts a rotated in
    [ Transformation.create ~rotation ~shift ]
  in
  List.find transformations ~f:(fun transformation ->
      let b_as_viewed_from_scanner_a =
        List.map b ~f:(Transformation.apply transformation)
      in
      count_overlap a_set (Int_triple.Set.of_list b_as_viewed_from_scanner_a) >= 12)
;;

let construct_graph beacons : (int * _) list Int.Map.t =
  let alist = Map.to_alist beacons in
  Int.Map.of_alist_multi
    (List.cartesian_product alist alist
    |> List.filter_map ~f:(fun ((i_a, beacons_a), (i_b, beacons_b)) ->
           Option.map (find_transformation beacons_a beacons_b) ~f:(fun v ->
               i_a, (i_b, v))))
;;

module Input' = struct
  type t = Int_triple.t list Int.Map.t

  let parser =
    let open Angstrom in
    let scanner_number =
      string "--- scanner " *> parse_int
      <* string " ---"
      <* end_of_line
      <?> "scanner number"
    in
    let parse_int_positive_or_negative =
      map2
        (option Sign.Pos (char '-' *> return Sign.Neg))
        parse_int
        ~f:(fun sign n -> n * Sign.to_int sign)
    in
    let beacon =
      map3
        (parse_int_positive_or_negative <* char ',')
        (parse_int_positive_or_negative <* char ',')
        parse_int_positive_or_negative
        ~f:Tuple3.create
      <?> "beacon"
    in
    let beacons = sep_by1 end_of_line beacon <?> "beacons" in
    let one = both scanner_number beacons in
    let alist = sep_by1 (end_of_line <* end_of_line) one in
    map alist ~f:Int.Map.of_alist_exn
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (Input')
  module Output = Int

  let dfs input ~f =
    let graph = construct_graph input in
    let visited = Int.Hash_set.create () in
    let rec dfs current_index transformation =
      match Hash_set.mem visited current_index with
      | true -> ()
      | false ->
        Hash_set.add visited current_index;
        f ~current_index ~transformation;
        List.iter (Map.find_exn graph current_index) ~f:(fun (index, transformation') ->
            dfs index (Transformation.combine transformation' transformation))
    in
    dfs 0 Transformation.zero
  ;;
end

module Part_01 = struct
  include Common

  let solve input =
    let beacons = Int_triple.Hash_set.create () in
    dfs input ~f:(fun ~current_index ~transformation ->
        let current_beacons_in_relative_coordinate_scheme =
          Map.find_exn input current_index
        in
        let beacons_in_root_coordinate_scheme =
          List.map
            current_beacons_in_relative_coordinate_scheme
            ~f:(Transformation.apply transformation)
        in
        List.iter beacons_in_root_coordinate_scheme ~f:(Hash_set.add beacons));
    Hash_set.length beacons
  ;;
end

module Part_02 = struct
  include Common

  let solve input =
    let scanners = Int_triple.Hash_set.create () in
    dfs input ~f:(fun ~current_index:_ ~transformation ->
        Hash_set.add scanners (Transformation.apply transformation Int_triple.zero));
    let scanners = Hash_set.to_list scanners in
    List.cartesian_product scanners scanners
    |> List.map ~f:(Tuple2.uncurry Int_triple.manhattan_distance)
    |> List.max_elt ~compare
    |> Option.value_exn
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]

let%test_module _ =
  (module struct
    open Common

    let a =
      [ 404, -588, -901
      ; 528, -643, 409
      ; -838, 591, 734
      ; 390, -675, -793
      ; -537, -823, -458
      ; -485, -357, 347
      ; -345, -311, 381
      ; -661, -816, -575
      ; -876, 649, 763
      ; -618, -824, -621
      ; 553, 345, -567
      ; 474, 580, 667
      ; -447, -329, 318
      ; -584, 868, -557
      ; 544, -627, -890
      ; 564, 392, -477
      ; 455, 729, 728
      ; -892, 524, 684
      ; -689, 845, -530
      ; 423, -701, 434
      ; 7, -33, -71
      ; 630, 319, -379
      ; 443, 580, 662
      ; -789, 900, -551
      ; 459, -707, 401
      ]
    ;;

    let b =
      [ 686, 422, 578
      ; 605, 423, 415
      ; 515, 917, -361
      ; -336, 658, 858
      ; 95, 138, 22
      ; -476, 619, 847
      ; -340, -569, -846
      ; 567, -361, 727
      ; -460, 603, -452
      ; 669, -402, 600
      ; 729, 430, 532
      ; -500, -761, 534
      ; -322, 571, 750
      ; -466, -666, -811
      ; -429, -592, 574
      ; -355, 545, -477
      ; 703, -491, -529
      ; -328, -685, 520
      ; 413, 935, -424
      ; -391, 539, -444
      ; 586, -435, 557
      ; -364, -763, -893
      ; 807, -499, -711
      ; 755, -354, -619
      ; 553, 889, -390
      ]
    ;;

    let%expect_test _ =
      let transformation = Option.value_exn (find_transformation a b) in
      let b_as_viewed_from_scanner_a =
        List.map b ~f:(Transformation.apply transformation)
      in
      let intersection =
        Set.inter
          (Int_triple.Set.of_list a)
          (Int_triple.Set.of_list b_as_viewed_from_scanner_a)
      in
      print_s [%sexp (intersection : Int_triple.Set.t)];
      [%expect
        {|
        ((-661 -816 -575) (-618 -824 -621) (-537 -823 -458) (-485 -357 347)
         (-447 -329 318) (-345 -311 381) (390 -675 -793) (404 -588 -901)
         (423 -701 434) (459 -707 401) (528 -643 409) (544 -627 -890)) |}]
    ;;

    let test_case =
      Input.of_string
        {|--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14|}
    ;;

    let%expect_test "Part 1" =
      print_s [%sexp (Part_01.solve test_case : int)];
      [%expect {| 79 |}]
    ;;

    let%expect_test "Part 2" =
      print_s [%sexp (Part_02.solve test_case : int)];
      [%expect {| 3621 |}]
    ;;
  end)
;;
