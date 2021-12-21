module Int = BatInt
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
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

let memo_rec h f =
  let rec g v =
    match Hashtbl.find h v with
    | exception Not_found ->
        let r = f g v in
        Hashtbl.add h v r;
        r
    | r -> r
  in
  g

module Day21 = struct
  let nroll_by_turn = 3

  module Part1 = struct
    let roll_dice dice =
      let rec aux dice move nroll =
        if nroll = nroll_by_turn then (move, dice)
        else
          let dice = dice + 1 in
          let dice = if dice = 101 then 1 else dice in
          aux dice (move + dice) (nroll + 1)
      in
      aux dice 0 0

    let move pos nmove =
      let move_mod = (nmove + pos) mod 10 in
      if move_mod = 0 then 10 else move_mod

    let play_turn total_roll dice (pos, score) =
      let nmove, dice = roll_dice dice in
      let pos = move pos nmove in
      (total_roll + nroll_by_turn, dice, (pos, score + pos))

    let play_game p1_pos p2_pos =
      let rec aux dice total_roll p1 p2 =
        if snd p1 >= 1000 then (total_roll, snd p2)
        else if snd p2 >= 1000 then (total_roll, snd p1)
        else
          let total_roll, dice, p1 = play_turn total_roll dice p1 in
          aux dice total_roll p2 p1
      in
      aux 0 0 (p1_pos, 0) (p2_pos, 0)
  end

  module Part2 = struct
    let sums =
      let a = ref IntMap.empty in
      for i = 1 to 3 do
        for j = 1 to 3 do
          for k = 1 to 3 do
            a :=
              IntMap.update
                (i + j + k)
                (function Some prev -> Some (prev + 1) | None -> Some 1)
                !a
          done
        done
      done;
      IntMap.bindings !a

    let play_turns (pos, score) =
      List.map
        (fun (nmove, mul) ->
          let pos = Part1.move pos nmove in
          ((pos, pos + score), mul))
        sums

    let play_game p1_pos p2_pos =
      let aux aux (is_p1, p1, p2) =
        if snd p1 >= 21 then if is_p1 then (1, 0) else (0, 1)
        else if snd p2 >= 21 then if is_p1 then (0, 1) else (1, 0)
        else
          let p1_universes = play_turns p1 in
          List.fold_left
            (fun (p1win_acc, p2win_acc) (p1, mul) ->
              let p1win, p2win = aux (not is_p1, p2, p1) in
              ((p1win * mul) + p1win_acc, p2win + p2win_acc))
            (0, 0) p1_universes
      in
      let aux = memo_rec (Hashtbl.create 17) aux in
      let p1win, p2win = aux (true, (p1_pos, 0), (p2_pos, 0)) in
      max p1win p2win
  end

  let run () =
    let p1 = 10 in
    let p2 = 9 in

    let total_roll, loser_score = Part1.play_game p1 p2 in
    Format.printf "%d * %d = %d @\n" total_roll loser_score
      (total_roll * loser_score);

    Format.printf "%d@\n" (Part2.play_game p1 p2);
    ()
  ;;

  run ()
end
