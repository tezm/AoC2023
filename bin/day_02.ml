(*--- Day 2: Cube Conundrum ---*)
open! Core

let constrains =
  [ 12, Re.Perl.compile_pat {|(\d+) red|}
  ; 13, Re.Perl.compile_pat {|(\d+) green|}
  ; 14, Re.Perl.compile_pat {|(\d+) blue|}
  ]
;;

let () =
  let input = Aoc2023.read_lines "./inputs/day_02.input" in
  let check_constrain line (num, re) =
    let matches = Re.all re line in
    match
      List.find matches ~f:(fun m ->
        Re.Group.get m 1 |> Int.of_string |> ( < ) num)
    with
    | Some _ -> true
    | None -> false
  in
  List.filter_mapi input ~f:(fun idx line ->
    match List.find constrains ~f:(check_constrain line) with
    | Some _ -> None
    | None -> Some (idx + 1))
  |> List.fold ~init:0 ~f:( + )
  |> Format.printf "Part 1: %d@."
;;
