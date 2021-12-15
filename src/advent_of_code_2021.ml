open! Core_kernel
open! Import

let days =
  [ Day01.parts
  ; Day02.parts
  ; Day03.parts
  ; Day04.parts
  ; Day05.parts
  ; Day06.parts
  ; Day07.parts
  ; Day08.parts
  ; Day09.parts
  ; Day10.parts
  ; Day11.parts
  ; Day12.parts
  ; Day13.parts
  ; Day14.parts
  ; Day15.parts
  ]
;;

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.mapi days ~f:(fun day_of_month_zero_indexed parts ->
         let day_of_month = day_of_month_zero_indexed + 1 in
         let command =
           Command.group
             ~summary:"Solve the selected part"
             (List.mapi parts ~f:(fun index (module Part) ->
                  let command =
                    Command.basic
                      ~summary:"Solve the puzzle"
                      (let%map_open.Command file =
                         flag
                           "-filename"
                           (optional_with_default
                              (sprintf "input/day%s.txt" (pad_int day_of_month))
                              string)
                           ~doc:" "
                       in
                       fun () ->
                         Part.Input.load file
                         |> Part.solve
                         |> Part.Output.to_string
                         |> printf "%s\n")
                  in
                  pad_int (index + 1), command))
         in
         pad_int day_of_month, command))
;;
