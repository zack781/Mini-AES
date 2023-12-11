
let read ic =
  let rec read_rec ic_rec acc =
    match input_line ic_rec with
    | line -> read_rec ic_rec (line :: acc)
    | exception End_of_file -> acc
  in
  read_rec ic [] |> List.rev |> String.concat "\n"

let parse file =
  let ic = open_in file in
  let s = read ic in
  close_in ic;
  s
