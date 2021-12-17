open! Core_kernel
open! Import

let parse_bits n =
  let open Angstrom in
  let one_bit =
    choice [ char '0' *> return 0; char '1' *> return 1; fail "Not a valid bit" ]
  in
  count n one_bit
  |> map ~f:(fun bits -> List.fold bits ~init:0 ~f:(fun acc bit -> (acc * 2) + bit))
;;

let reparse (parser : 'a Angstrom.t) (string_parser : string Angstrom.t) : 'a Angstrom.t =
  let open Angstrom in
  bind string_parser ~f:(fun s ->
      match parse_string parser s ~consume:All with
      | Ok v -> return v
      | Error error -> fail error)
;;

module Type_id = struct
  type t =
    | Number
    | Operator of int
  [@@deriving sexp]

  let of_int = function
    | 4 -> Number
    | n -> Operator n
  ;;

  let parser = parse_bits 3 |> Angstrom.map ~f:of_int
end

module Packet_header = struct
  type t =
    { version : int
    ; type_id : Type_id.t
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    map2 (parse_bits 3) Type_id.parser ~f:(fun version type_id -> { version; type_id })
  ;;
end

module Number_body = struct
  type t = int

  let parser =
    let open Angstrom in
    let first_n = many (char '1' *> parse_bits 4) in
    let last = char '0' *> parse_bits 4 in
    map2 first_n last ~f:(fun first_n last ->
        (List.fold first_n ~init:0 ~f:(fun acc body -> (acc lsl 4) + body) lsl 4) + last)
  ;;
end

module Operator_body = struct
  type 'packet t = 'packet list

  let parser packet_parser =
    let open Angstrom in
    bind
      (choice
         [ char '0' *> return `zero; char '1' *> return `one; fail "Invalid length type" ])
      ~f:(fun length_type ->
        match length_type with
        | `zero ->
          parse_bits 15
          |> bind ~f:(fun bit_length -> take bit_length |> reparse (many1 packet_parser))
        | `one ->
          parse_bits 11 |> bind ~f:(fun num_packets -> count num_packets packet_parser))
    <?> "subpackets"
  ;;
end

module Packet_body = struct
  type 'packet t =
    | Number of int
    | Operator of
        { operator_id : int
        ; packets : 'packet list
        }
  [@@deriving sexp]
end

module Packet = struct
  type t =
    { version : int
    ; body : t Packet_body.t
    }
  [@@deriving sexp]

  let parser =
    let open Angstrom in
    fix (fun parser ->
        bind Packet_header.parser ~f:(fun { version; type_id } ->
            map
              (match type_id with
              | Number -> map Number_body.parser ~f:(fun n -> Packet_body.Number n)
              | Operator operator_id ->
                Operator_body.parser parser
                |> map ~f:(fun packets -> Packet_body.Operator { operator_id; packets }))
              ~f:(fun body -> { version; body })))
    <* many (char '0')
  ;;
end

module Bits = struct
  type t = string

  let parser =
    let open Angstrom in
    let parse_hex_char =
      bind any_char ~f:(function
          | '0' -> return "0000"
          | '1' -> return "0001"
          | '2' -> return "0010"
          | '3' -> return "0011"
          | '4' -> return "0100"
          | '5' -> return "0101"
          | '6' -> return "0110"
          | '7' -> return "0111"
          | '8' -> return "1000"
          | '9' -> return "1001"
          | 'A' -> return "1010"
          | 'B' -> return "1011"
          | 'C' -> return "1100"
          | 'D' -> return "1101"
          | 'E' -> return "1110"
          | 'F' -> return "1111"
          | _ -> fail "Not a hex digit")
    in
    map (many1 parse_hex_char) ~f:String.concat
  ;;
end

module Common = struct
  module Input = Input.Make_parseable (struct
    type t = Packet.t

    let parser = reparse Packet.parser Bits.parser
  end)

  module Output = Int
end

module Part_01 = struct
  include Common

  let rec solve ({ version; body } : Input.t) =
    version
    +
    match body with
    | Number _ -> 0
    | Operator { packets; operator_id = _ } -> List.sum (module Int) packets ~f:solve
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01) ]

let%test_module _ =
  (module struct
    open Common

    let%expect_test "Parsing" =
      let test input =
        let test_case = Input.of_string input in
        print_s [%sexp (test_case : Packet.t)]
      in
      test "D2FE28";
      [%expect {| ((version 6) (body (Number 2021))) |}];
      test "38006F45291200";
      [%expect
        {|
        ((version 1)
         (body
          (Operator (operator_id 6)
           (packets
            (((version 6) (body (Number 10))) ((version 2) (body (Number 20)))))))) |}];
      test "EE00D40C823060";
      [%expect
        {|
        ((version 7)
         (body
          (Operator (operator_id 3)
           (packets
            (((version 2) (body (Number 1))) ((version 4) (body (Number 2)))
             ((version 1) (body (Number 3)))))))) |}]
    ;;

    let%expect_test "Part 1" =
      let test input =
        let test_case = Input.of_string input in
        print_s [%sexp (Part_01.solve test_case : int)]
      in
      test "8A004A801A8002F478";
      [%expect {| 16 |}];
      test "620080001611562C8802118E34";
      [%expect {| 12 |}];
      test "C0015000016115A2E0802F182340";
      [%expect {| 23 |}];
      test "A0016C880162017C3686B18A3D4780";
      [%expect {| 31 |}]
    ;;
  end)
;;
