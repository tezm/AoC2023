(*--- Day 1: Trebuchet?! ---*)
open! Core

let rec find_digit list =
  match list with
  | x :: _ when Char.is_digit x -> x
  | _ :: xs -> find_digit xs
  | [] -> '0'
;;

let ch_to_int ch = Char.to_int ch - 48

let extract_number line =
  let tens = (ch_to_int @@ find_digit @@ String.to_list @@ line) * 10 in
  let ones = ch_to_int @@ find_digit @@ String.to_list_rev @@ line in
  tens + ones
;;

let () =
  let input = Aoc2023.read_lines "day_01.input" in
  Format.printf "Part 1: %d@."
  @@ List.fold ~init:0 ~f:( + )
  @@ List.map ~f:extract_number
  @@ input
;;

let dict =
  [ "one", 1
  ; "two", 2
  ; "three", 3
  ; "four", 4
  ; "five", 5
  ; "six", 6
  ; "seven", 7
  ; "eight", 8
  ; "nine", 9
  ; "1", 1
  ; "2", 2
  ; "3", 3
  ; "4", 4
  ; "5", 5
  ; "6", 6
  ; "7", 7
  ; "8", 8
  ; "9", 9
  ]
;;

let match_from_dict str pos =
  List.find_map dict ~f:(fun (substr, value) ->
    let re = Str.regexp_string substr in
    match Str.string_match re str pos with
    | true -> Some value
    | false -> None)
;;

let get_numbers_in_string str =
  List.filter_map ~f:(match_from_dict str) @@ List.range 0 (String.length str)
;;

let () =
  let input = Aoc2023.read_lines "day_01.input" in
  List.fold input ~init:0 ~f:(fun acc str ->
    let numbs = get_numbers_in_string str in
    let digits = List.hd_exn numbs in
    let ones = List.last_exn numbs in
    acc + (digits * 10) + ones)
  |> Format.printf "Part 2: %d@."
;;
