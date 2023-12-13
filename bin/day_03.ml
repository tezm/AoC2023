(*--- Day 3: Gear Ratios ---*)
open! Core

let re_number = Re.Perl.compile_pat {|\d+|}

let () =
  let input = Aoc2023.read_lines "./inputs/day_03.input" in
  let dot_line = String.make (List.hd_exn input |> String.length) '.' in
  let input_better = [ dot_line ] @ input @ [ dot_line ] in
  let rec check_for_symbol str start stop =
    if start >= String.length str || stop < 0 || start >= stop
    then false
    else if start < 0
    then check_for_symbol str 0 stop
    else (
      match String.get str start with
      | '0' .. '9' -> check_for_symbol str (start + 1) stop
      | '.' -> check_for_symbol str (start + 1) stop
      | _ -> true)
  in
  let rec find_serials text init =
    match text with
    | above :: line :: under ->
      let line_sum =
        Re.all re_number line
        |> List.filter ~f:(fun num ->
          check_for_symbol line (Re.Group.stop num 0) (Re.Group.stop num 0 + 1)
          || check_for_symbol
               line
               (Re.Group.start num 0 - 1)
               (Re.Group.start num 0)
          || check_for_symbol
               above
               (Re.Group.start num 0 - 1)
               (Re.Group.stop num 0 + 1)
          || check_for_symbol
               (List.hd_exn under)
               (Re.Group.start num 0 - 1)
               (Re.Group.stop num 0 + 1))
        |> List.map ~f:(fun num -> Re.Group.get num 0 |> Int.of_string)
        |> List.fold ~init:0 ~f:(fun acc num -> num + acc)
      in
      find_serials (line :: under) (init + line_sum)
    | _ :: [] -> init
    | [] -> init
  in
  find_serials input_better 0 |> Format.printf "Part 1 outpu: %d@."
;;
