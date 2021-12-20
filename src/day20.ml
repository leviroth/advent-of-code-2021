open! Core_kernel
open Import

let set_bounds set =
  let row_indexes, col_indexes = List.unzip (Set.to_list set) in
  ( ( Option.value_exn (List.min_elt row_indexes ~compare)
    , Option.value_exn (List.max_elt row_indexes ~compare) )
  , ( Option.value_exn (List.min_elt col_indexes ~compare)
    , Option.value_exn (List.max_elt col_indexes ~compare) ) )
;;

module Image = struct
  type t =
    { explicit_pixels : Int_pair.Set.t
    ; row_lo : int
    ; row_hi : int
    ; col_lo : int
    ; col_hi : int
    ; outer : bool
    }

  let get { explicit_pixels; row_lo; row_hi; col_lo; col_hi; outer } (row, col) =
    match row_lo <= row && row <= row_hi && col_lo <= col && col <= col_hi with
    | true -> Set.mem explicit_pixels (row, col)
    | false -> outer
  ;;

  let create ~explicit_pixels ~outer =
    let (row_lo, row_hi), (col_lo, col_hi) = set_bounds explicit_pixels in
    { explicit_pixels; row_lo; row_hi; col_lo; col_hi; outer }
  ;;
end

module Input' = struct
  type t =
    { algorithm : bool array
    ; image : Image.t
    }

  let parser =
    let open Angstrom in
    let pixel = choice [ char '#' *> return true; char '.' *> return false ] in
    let algorithm =
      sep_by1 end_of_line (many1 pixel)
      <* end_of_line
      <* end_of_line
      |> map ~f:(fun ls -> List.concat ls |> Array.of_list)
    in
    let one_row = many1 pixel in
    let all_rows = sep_by1 end_of_line one_row in
    let pixels =
      map all_rows ~f:(fun rows ->
          List.concat_mapi rows ~f:(fun row_idx row ->
              List.filter_mapi row ~f:(fun col_idx pixel ->
                  match pixel with
                  | false -> None
                  | true -> Some (row_idx, col_idx)))
          |> Int_pair.Set.of_list)
    in
    map2 algorithm pixels ~f:(fun algorithm pixels ->
        { algorithm; image = Image.create ~explicit_pixels:pixels ~outer:false })
  ;;
end

let output_indexes_to_consider ({ row_lo; row_hi; col_lo; col_hi; _ } : Image.t) =
  List.range ~stop:`inclusive (row_lo - 1) (row_hi + 1)
  |> List.concat_map ~f:(fun row_idx ->
         List.range ~stop:`inclusive (col_lo - 1) (col_hi + 1)
         |> List.map ~f:(fun col_idx -> row_idx, col_idx))
;;

let algorithm_index_of_pixel (row, col) image =
  let neighbors =
    List.range ~stop:`inclusive (row - 1) (row + 1)
    |> List.map ~f:(fun row ->
           List.range ~stop:`inclusive (col - 1) (col + 1)
           |> List.map ~f:(fun col -> Image.get image (row, col)))
    |> List.concat
  in
  List.fold neighbors ~init:0 ~f:(fun acc pixel ->
      (2 * acc)
      +
      match pixel with
      | true -> 1
      | false -> 0)
;;

let get_output algorithm image =
  let new_explicit_pixels =
    List.filter (output_indexes_to_consider image) ~f:(fun coords ->
        algorithm.(algorithm_index_of_pixel coords image))
    |> Int_pair.Set.of_list
  in
  Image.create
    ~explicit_pixels:new_explicit_pixels
    ~outer:
      algorithm.(match image.outer with
                 | true -> 511
                 | false -> 0)
;;

module Common = struct
  module Input = Input.Make_parseable (Input')
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve' ({ algorithm; image } : Input.t) =
    Fn.apply_n_times ~n:2 (get_output algorithm) image
  ;;

  let solve input = Set.length (solve' input).explicit_pixels
end

let parts : (module Solution.Part) list = [ (module Part_01) ]

let%test_module _ =
  (module struct
    open Common

    let test_case =
      Input.of_string
        {|..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###|}
    ;;

    let%expect_test _ =
      print_s [%sexp (Part_01.solve test_case : int)];
      [%expect {| 35 |}]
    ;;
  end)
;;
