module Int = BatInt
module IntSet = Set.Make (Int)
module CharSet = Set.Make (Char)
module CharSetMap = Map.Make (CharSet)
module FloatSet = Set.Make (Float)

module Point = struct
  type t = int * int

  let compare = compare
end

module PtSet = Set.Make (Point)

exception Exitv of int

let input_error () = failwith "wrong input"

let () = print_endline "Advent of code"

let num_of_char c = Char.code c - 48

let foldf f init file =
  let ic = open_in file in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> acc
    | line -> loop (f acc line)
  in
  let r = loop init in
  close_in ic;
  r

let print_arr f fmt a = Array.iter (fun v -> Format.fprintf fmt "%a" f v) a

let print_opt f fmt o =
  match o with
  | Some v -> Format.fprintf fmt "Some %a" f v
  | None -> Format.fprintf fmt "None"

let print_mat f fmt m =
  Array.iter (fun col -> Format.fprintf fmt "%a @\n" (print_arr f) col) m

let print_seq f fmt seq =
  Seq.iter (fun e -> Format.fprintf fmt "%a" f e) seq;
  Format.fprintf fmt "@\n"

let printl f fmt l = l |> List.to_seq |> print_seq f fmt

let parse_matrix f file =
  foldf
    (fun acc line ->
      let line =
        String.to_seq line
        |> Seq.fold_left (fun acc c -> f c :: acc) []
        |> List.rev |> Array.of_list
      in
      line :: acc)
    [] file
  |> List.rev |> Array.of_list

let visit_point board f x y acc =
  match board.(y).(x) with
  | exception Invalid_argument _ -> acc
  | v -> f acc (x, y) v

let visit_4neighbours acc board (x, y) f =
  let visit = visit_point board in
  visit f (x + 1) y acc
  |> visit f (x - 1) y
  |> visit f x (y - 1)
  |> visit f x (y + 1)

let update_tbl h k default f =
  let v =
    match Hashtbl.find h k with exception Not_found -> default | v -> f v
  in
  Hashtbl.replace h k v
