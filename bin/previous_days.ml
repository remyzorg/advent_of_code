module Day20 = struct
  let img_of_string strl =
    List.map
      (fun str -> Array.init (String.length str) (fun i -> str.[i] = '#'))
      strl
    |> Array.of_list

  let considered_pixels_val neutral a (x, y) =
    let get y x = visit_point a (fun _ _ v -> v) x y neutral in
    Bitv.init 63 (function
      | 8 -> get (y - 1) (x - 1)
      | 7 -> get (y - 1) x
      | 6 -> get (y - 1) (x + 1)
      | 5 -> get y (x - 1)
      | 4 -> get y x
      | 3 -> get y (x + 1)
      | 2 -> get (y + 1) (x - 1)
      | 1 -> get (y + 1) x
      | 0 -> get (y + 1) (x + 1)
      | _ -> false)
    |> Bitv.to_int_s

  let growth neutral img =
    let n = 2 in
    let growth empty a f =
      Array.init
        (Array.length a + n)
        (fun i ->
          match a.(i - (n / 2)) with
          | exception Invalid_argument _ -> empty ()
          | v -> f v)
    in
    growth
      (fun () -> Array.make (Array.length img.(0) + n) neutral)
      img
      (fun v -> growth (fun () -> neutral) v (fun x -> x))

  let enhance neutral algo img =
    let img = growth neutral img in
    Array.init (Array.length img) (fun y ->
        Array.init
          (Array.length img.(0))
          (fun x ->
            let val_in_algo = considered_pixels_val neutral img (x, y) in
            algo.[val_in_algo] = '#'))

  let count_light img =
    Array.fold_left
      (fun acc col ->
        Array.fold_left (fun acc v -> if v then acc + 1 else acc) acc col)
      0 img

  let parse file =
    let _, algo, img =
      foldf
        (fun (parse_img, algo, img) line ->
          if parse_img then (parse_img, algo, line :: img)
          else if line = "" then (true, algo, img)
          else (parse_img, line :: algo, img))
        (false, [], []) file
    in
    (algo |> List.rev |> String.concat "", img |> List.rev)

  let incr_neutral algo neutral =
    ((not neutral) && algo.[0] = '#') || (neutral && algo.[511] = '#')

  let enhance_n algo img n0 =
    let rec aux neutral n img =
      if n = n0 then img
      else aux (incr_neutral algo neutral) (n + 1) (enhance neutral algo img)
    in
    aux false 0 img

  let run () =
    let algo, img = parse "day20.txt" in
    let img = img_of_string img in
    let enhanced = enhance_n algo img 50 in
    Format.printf "light: %d@\n" (count_light enhanced);
    ()
end

module Day19 = struct
  let parse file =
    let current, scanners =
      foldf
        (fun ((current, scanners) as acc) line ->
          if line = "" then acc
          else if line.[1] = '-' then
            if current = [] then acc else ([], List.rev current :: scanners)
          else if String.contains line ',' then
            match List.map int_of_string @@ String.split_on_char ',' line with
            | [ x; y; z ] -> ((x, y, z) :: current, scanners)
            | _ -> input_error ()
          else input_error ())
        ([], []) file
    in
    List.rev (List.rev current :: scanners)

  let print_bcn fmt (x, y, z) = Format.fprintf fmt "%d,%d,%d" x y z

  type bcn = int * int * int

  module BeaconRelSet = Set.Make (struct
    type t = (int * bcn) * (int * bcn)

    let compare = compare
  end)

  module BeaconSet = Set.Make (struct
    type t = bcn * int

    let compare = compare
  end)

  let print scanners =
    List.iteri
      (fun n beacons ->
        Format.printf "--- scanner %d ---@\n" n;
        List.iter (fun bcn -> Format.printf "%a@\n" print_bcn bcn) beacons;
        Format.printf "@\n")
      scanners

  let dist (x1, y1, z1) (x2, y2, z2) =
    sqrt
      (Float.pow (float x2 -. float x1) 2.
      +. Float.pow (float y2 -. float y1) 2.
      +. Float.pow (float z2 -. float z1) 2.)

  let find_distances scanner =
    List.map
      (fun bcn ->
        let set =
          List.fold_left
            (fun acc bcn' ->
              let dist = dist bcn bcn' in
              if bcn = bcn' then acc else FloatSet.add dist acc)
            FloatSet.empty scanner
        in
        (bcn, set))
      scanner

  let flatten_distances scanners =
    List.mapi
      (fun sci sc ->
        List.map (fun (bcn, dists) -> ((sci, bcn), dists)) (find_distances sc))
      scanners
    |> List.flatten

  let normal (sc1, _) (sc2, _) = compare sc1 sc2 < 0

  let normalize bcn1 bcn2 =
    if normal bcn1 bcn2 then (bcn1, bcn2) else (bcn2, bcn1)

  let find_relations distances =
    List.fold_left
      (fun acc (bcn1, dists) ->
        List.fold_left
          (fun acc (bcn2, dists') ->
            let bcn1, bcn2 = normalize bcn1 bcn2 in
            if fst bcn1 = fst bcn2 || BeaconRelSet.mem (bcn1, bcn2) acc then acc
            else
              let inter_points = FloatSet.inter dists dists' in
              if FloatSet.cardinal inter_points >= 11 then
                BeaconRelSet.add (bcn1, bcn2) acc
              else acc)
          acc distances)
      BeaconRelSet.empty distances

  type cl = Ref of (int * bcn) list | Mem of (int * bcn)

  let get_ref bcn = function Ref _ -> bcn | Mem bcn -> bcn

  let get_list h rf =
    match Hashtbl.find h rf with Mem _ -> assert false | Ref l -> l

  let find_uniques distances relations =
    let h = Hashtbl.create 17 in
    let add bcn1 bcn2 =
      match (Hashtbl.find_opt h bcn1, Hashtbl.find_opt h bcn2) with
      | None, None ->
          Hashtbl.add h bcn1 (Ref [ bcn2 ]);
          Hashtbl.add h bcn2 (Mem bcn1)
      | Some cl, None ->
          let rf = get_ref bcn1 cl in
          Hashtbl.add h bcn2 (Mem rf);
          update_tbl h rf
            (Ref [ bcn2 ])
            (function Ref l -> Ref (bcn2 :: l) | Mem _ -> assert false)
      | None, Some cl ->
          let rf = get_ref bcn2 cl in
          if normal rf bcn1 then (
            Hashtbl.add h bcn1 (Mem rf);
            update_tbl h rf
              (Ref [ bcn1 ])
              (function Ref l -> Ref (bcn1 :: l) | Mem _ -> assert false))
          else
            let l = rf :: get_list h rf in
            Hashtbl.add h bcn1 (Ref l);
            List.iter (fun bcn -> Hashtbl.replace h bcn (Mem bcn1)) l
      | Some cl1, Some cl2 ->
          let rf1 = get_ref bcn1 cl1 in
          let rf2 = get_ref bcn2 cl2 in
          if rf1 = rf2 then ()
          else
            let rf1, bcn1, rf2, _ =
              if normal rf1 rf2 then (rf1, bcn1, rf2, bcn2)
              else (rf2, bcn2, rf1, bcn1)
            in
            let l1 = get_list h rf1 in
            let l2 = get_list h rf2 in
            let l2 = rf2 :: l2 in
            let l = l1 @ l2 in
            update_tbl h bcn1 (Ref l) (fun _ -> Ref l);
            List.iter (fun bcn -> Hashtbl.replace h bcn (Mem bcn1)) l2
    in
    BeaconRelSet.iter (fun (bcn1, bcn2) -> add bcn1 bcn2) relations;
    List.map
      (fun (bcn, _) ->
        match Hashtbl.find h bcn with
        | exception Not_found -> bcn
        | r -> get_ref bcn r)
      distances
    |> List.sort_uniq compare

  let run () =
    let scanners = parse "day19.txt" in
    (* print scanners; *)
    let distances = flatten_distances scanners in
    let relations = find_relations distances in
    let uniques = find_uniques distances relations in
    Format.printf "%a@\n"
      (print_seq (fun fmt ((sc1, bcn1), (sc2, bcn2)) ->
           Format.fprintf fmt "(%d|%a), (%d|%a) @\n" sc1 print_bcn bcn1 sc2
             print_bcn bcn2))
      (BeaconRelSet.elements relations
      |> List.sort (fun ((sc1, _), (sc2, _)) ((sc1', _), (sc2', _)) ->
             let cmp = compare sc1 sc1' in
             if cmp = 0 then compare sc2 sc2' else cmp)
      |> List.to_seq);
    Format.printf "distances: %d@\n" (List.length distances);
    Format.printf "relations: %d@\n" (BeaconRelSet.cardinal relations);
    Format.printf "total: %d@\n" (List.length uniques);
    ()
end

module Day18 = struct
  type t = Pair of pair | Num of int

  and pair = { depth : int; l : t; r : t }

  let get_depth = function Pair p -> p.depth | Num _ -> 0

  let mk_pair ~l ~r =
    let depth = max (get_depth l) (get_depth r) + 1 in
    { depth; l; r }

  let of_string s =
    let rec aux i =
      match s.[i] with
      | exception Invalid_argument _ -> failwith "input error"
      | '0' .. '9' as c -> (Num (num_of_char c), i + 1)
      | '[' ->
          let l, i = aux (i + 1) in
          assert (s.[i] = ',');
          let r, i = aux (i + 1) in
          assert (s.[i] = ']');
          (Pair (mk_pair ~l ~r), i + 1)
      | _ -> failwith "input error"
    in
    aux 0 |> fst

  let rec print fmt t =
    match t with
    | Num n -> Format.fprintf fmt "%d" n
    | Pair { l; r; _ } -> Format.fprintf fmt "[%a,%a]" print l print r

  let rec add_leftest n = function
    | Pair { l; r; _ } -> Pair (mk_pair ~l:(add_leftest n l) ~r)
    | Num m -> Num (n + m)

  let rec add_rightest n = function
    | Pair { l; r; _ } -> Pair (mk_pair ~l ~r:(add_rightest n r))
    | Num m -> Num (n + m)

  let explode t =
    let rec aux depth_to_explode t =
      match t with
      | Pair { depth; l = Num nl; r = Num nr }
        when depth_to_explode = 1 && depth = 1 ->
          (Num 0, true, Some nl, Some nr)
      | Pair { depth; l; r } when depth = depth_to_explode ->
          let l, exploded, rest_l, rest_r = aux (depth_to_explode - 1) l in
          if exploded then
            let r = match rest_r with Some n -> add_leftest n r | None -> r in
            (Pair (mk_pair ~l ~r), true, rest_l, None)
          else
            let r, exploded, rest_l, rest_r = aux (depth_to_explode - 1) r in
            let l =
              match rest_l with Some n -> add_rightest n l | None -> l
            in
            (Pair (mk_pair ~l ~r), exploded, None, rest_r)
      | t -> (t, false, None, None)
    in
    let res, exploded, _, _ = aux 5 t in
    (res, exploded)

  let split t =
    let rec aux = function
      | Num n when n >= 10 ->
          let l = n / 2 in
          let r = Num (n - l) in
          (true, Pair (mk_pair ~l:(Num l) ~r))
      | Pair { l = old_l; r = old_r; _ } as p ->
          let splitted, l = aux old_l in
          if splitted then (splitted, Pair (mk_pair ~l ~r:old_r))
          else
            let splitted, r = aux old_r in
            (splitted, if splitted then Pair (mk_pair ~l ~r) else p)
      | p -> (false, p)
    in
    aux t

  let rec reduce t =
    let t, exploded = explode t in
    if exploded then reduce t
    else
      let splitted, t = split t in
      if splitted then reduce t else t

  let add l r = Pair (mk_pair ~l ~r) |> reduce

  let rec magnitude = function
    | Pair { l; r; _ } -> (magnitude l * 3) + (magnitude r * 2)
    | Num n -> n

  let parse file =
    foldf (fun acc line -> of_string line :: acc) [] file |> List.rev

  let part1 () =
    let parsed = parse "day18.txt" in
    let parsed =
      List.fold_left
        (fun acc e ->
          match acc with Some acc -> Some (add acc e) | None -> Some e)
        None parsed
    in
    let parsed =
      match parsed with None -> failwith "input failed" | Some p -> p
    in
    Format.printf "%a@\n" print parsed;
    Format.printf "%d@\n" (magnitude parsed)

  let part2 () =
    let parsed = parse "day18.txt" in
    let a = Array.of_list parsed in
    let max_magn = ref min_int in
    Array.iter
      (fun n1 ->
        Array.iter
          (fun n2 ->
            if n1 == n2 then ()
            else
              let m1 = magnitude (add n1 n2) in
              let m2 = magnitude (add n1 n2) in
              max_magn := max m1 !max_magn;
              max_magn := max m2 !max_magn)
          a)
      a;
    Format.printf "%d@\n" !max_magn
end

module Day17 = struct
  let parse file =
    foldf
      (fun _ line ->
        match
          Scanf.sscanf line "target area: x=%d..%d, y=%d..%d"
            (fun xmin xmax ymin ymax -> ((xmin, xmax), (-ymax, -ymin)))
        with
        | res -> res
        | exception Scanf.Scan_failure _ -> failwith "scan failure")
      ((0, 0), (0, 0))
      file

  let in_inter (min, max) c = c >= min && c <= max

  let after_square (_, xmax) (x, _y) = x > xmax

  let under_square (_, ymax) (_x, y) = y > ymax

  type res = Under | After | In of (int * int) list

  let compute_pos inter_x inter_y pt vct =
    let rec aux acc ((x, y) as pt) (x_vct, y_vct) =
      if under_square inter_y pt then Under
      else if after_square inter_x pt then After
      else if in_inter inter_x x && in_inter inter_y y then In acc
      else
        let x = x + x_vct in
        let y = y - y_vct in
        let x_vct =
          if x_vct < 0 then x_vct + 1
          else if x_vct > 0 then x_vct - 1
          else x_vct
        in
        let y_vct = y_vct - 1 in
        aux ((x, y) :: acc) (x, y) (x_vct, y_vct)
    in
    aux [] pt vct

  let find_vcts inter_x inter_y =
    let rec aux acc ((x_vct, y_vct) as vct) =
      let acc =
        match compute_pos inter_x inter_y (0, 0) vct with
        | Under -> acc
        | After -> acc
        | In r -> r :: acc
      in
      if y_vct > snd inter_y then acc
      else if x_vct > snd inter_x then aux acc (1, y_vct + 1)
      else aux acc (x_vct + 1, y_vct)
    in
    aux [] (1, -snd inter_y)

  let run () =
    let inter_x, inter_y = parse "day17.txt" in
    let vcts = find_vcts inter_x inter_y in
    let miny =
      List.fold_left
        (fun acc l -> List.fold_left (fun acc (_, y) -> min y acc) acc l)
        max_int vcts
    in
    Format.printf "distinct vct: %d, max y: %d @\n" (List.length vcts) (-miny)
end

module Day16 = struct
  let bin_from_c = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> failwith "input_error"

  let reverse bv = bv |> Bitv.M.to_string |> Bitv.L.of_string

  let to_bitv v =
    let message = Bitv.create (String.length v * 4) false in
    String.iteri
      (fun i c ->
        let bv = bin_from_c c |> Bitv.L.of_string in
        Bitv.blit bv 0 message (i * 4) 4)
      v;
    message

  let parse file =
    foldf (fun acc l -> l :: acc) [] file |> function
    | [ v ] -> v
    | _ -> failwith "input error"

  let to_int bv =
    let z = Bitv.create 63 false in
    Bitv.blit (reverse bv) 0 z 0 (Bitv.length bv);
    Bitv.to_int_s z

  let sub_and_rm f bv len =
    (Bitv.sub bv 0 len |> f, Bitv.sub bv len (Bitv.length bv - len))

  type packets_lim = Packets of int | Vec of Bitv.t

  let apply_compare op = function
    | [ x; y ] -> if op x y then 1 else 0
    | _ -> failwith "input error"

  let apply_op = function
    | 0 -> List.fold_left ( + ) 0
    | 1 -> List.fold_left ( * ) 1
    | 2 -> List.fold_left min max_int
    | 3 -> List.fold_left max min_int
    | 5 -> apply_compare ( > )
    | 6 -> apply_compare ( < )
    | 7 -> apply_compare ( = )
    | _ -> failwith "input error"

  let rec decode_packets msg lim =
    match lim with
    | Packets n ->
        if n = 0 then ([], msg)
        else
          let res, msg = decode msg in
          let resl, msg = decode_packets msg (Packets (n - 1)) in
          (res :: resl, msg)
    | Vec sub_msg ->
        if Bitv.all_zeros sub_msg then ([], msg)
        else
          let res, sub_msg = decode sub_msg in
          let resl, msg = decode_packets msg (Vec sub_msg) in
          (res :: resl, msg)

  and decode_literal msg =
    let lit, msg = sub_and_rm (fun x -> x) msg 5 in
    let not_last = Bitv.get lit 0 in
    let _, lit = sub_and_rm (fun x -> x) lit 1 in
    if not_last then
      let tail, msg = decode_literal msg in
      (Bitv.append lit tail, msg)
    else (lit, msg)

  and decode msg =
    let _, msg = sub_and_rm to_int msg 3 in
    let tid, msg = sub_and_rm to_int msg 3 in
    let ltid, msg =
      if tid <> 4 then sub_and_rm (fun x -> Some (Bitv.get x 0)) msg 1
      else (None, msg)
    in
    match ltid with
    | Some ltid ->
        let lim, msg =
          if ltid then
            let nb, msg = sub_and_rm to_int msg 11 in
            (Packets nb, msg)
          else
            let nb, msg = sub_and_rm to_int msg 15 in
            let sub_msg, msg = sub_and_rm (fun x -> x) msg nb in
            (Vec sub_msg, msg)
        in
        let results, msg = decode_packets msg lim in
        (apply_op tid results, msg)
    | None ->
        let lit, msg = decode_literal msg in
        (to_int lit, msg)

  let run () =
    let v = parse "day16.txt" in
    let vec = to_bitv v in
    Format.printf "%s @\n" (Bitv.L.to_string vec);
    let v, _ = decode vec in
    Format.printf "res: %d @\n" v
end

module Day15 = struct
  module SortedPoint = struct
    type t = (int * int) * ((int * int) * int)

    let compare (_, (_, w1)) (_, (_, w2)) = compare w1 w2
  end

  module Heap = BatHeap.Make (SortedPoint)

  let rec sorted_insert e f l =
    match l with
    | [] -> [ e ]
    | h :: t when f e h < 0 -> e :: h :: t
    | h :: t when f e h > 0 -> h :: sorted_insert e f t
    | h :: t -> e :: h :: t

  let shortest_path grid start dest =
    let h = Hashtbl.create 17 in
    let rec aux heap =
      match Heap.find_min heap with
      | exception Invalid_argument _ -> ()
      | spot, (_prev, cum_weight) ->
          let heap = Heap.del_min heap in
          let heap =
            visit_4neighbours heap grid spot (fun acc nbr weight ->
                let cum_weight = weight + cum_weight in
                match Hashtbl.find h nbr with
                | exception Not_found ->
                    Hashtbl.replace h nbr (spot, cum_weight);
                    Heap.insert acc (nbr, (spot, cum_weight))
                | _, nbr_cum_weight when cum_weight < nbr_cum_weight ->
                    Hashtbl.replace h nbr (spot, cum_weight);
                    Heap.insert acc (nbr, (spot, cum_weight))
                | _ -> acc)
          in
          aux heap
    in
    aux (Heap.insert Heap.empty (start, (start, 0)));
    Hashtbl.find h dest |> snd

  let unfold_grid grid =
    Array.init
      (Array.length grid * 5)
      (fun y ->
        Array.init
          (Array.length grid.(0) * 5)
          (fun x ->
            let mody = y mod Array.length grid in
            let multy = (y - mody) / Array.length grid in
            let modx = x mod Array.length grid.(0) in
            let multx = (x - modx) / Array.length grid.(0) in
            let r = grid.(mody).(modx) + multy + multx in
            if r mod 9 = 0 then 9 else r mod 9))

  let run () =
    let grid = parse_matrix "day15.txt" in
    let grid = unfold_grid grid in
    Format.printf "%d@\n"
      (shortest_path grid (0, 0)
         (Array.length grid - 1, Array.length grid.(0) - 1))
end

module Day14 = struct
  let parse file =
    let rules = Hashtbl.create 17 in
    let init =
      foldf
        (fun init line ->
          match String.split_on_char ' ' line with
          | [ pair; "->"; element ] ->
              Hashtbl.add rules (pair.[0], pair.[1]) element.[0];
              init
          | [ s ] when s <> "" -> s
          | _ -> init)
        "" file
    in
    (init, rules)

  module Part1 = struct
    let apply_rules_once rules w =
      let rec aux acc w_seq =
        match w_seq with
        | Seq.Cons (h1, f) -> (
            match f () with
            | Cons (h2, _) as w_seq ->
                let acc =
                  match Hashtbl.find rules (h1, h2) with
                  | exception Not_found -> h1 :: acc
                  | c -> c :: h1 :: acc
                in
                aux acc w_seq
            | Nil -> h1 :: acc)
        | Nil -> acc
      in
      aux [] (String.to_seq w ())
      |> List.rev_map (String.make 1)
      |> String.concat ""

    let rec apply_rules n rules w =
      Format.printf "step %d@." n;
      if n = 0 then w
      else if n = 1 then apply_rules_once rules w
      else apply_rules (n - 1) rules (apply_rules_once rules w)

    let most_least_in_htbl h =
      Hashtbl.fold
        (fun k v (((mostc, mostv) as most), ((leastc, leastv) as least)) ->
          if v > mostv then ((k, v), least)
          else if v < leastv then (most, (k, v))
          else (most, least))
        h
        (('H', min_int), ('H', max_int))

    let most_least w =
      let h = Hashtbl.create 17 in
      String.iter
        (fun c ->
          let prev =
            match Hashtbl.find h c with exception Not_found -> 0 | n -> n
          in
          Hashtbl.replace h c (prev + 1))
        w;
      most_least_in_htbl h
  end

  module Part2 = struct
    module CharMap = Map.Make (Char)

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

    let most_least m =
      CharMap.fold
        (fun k v (((mostc, mostv) as most), ((leastc, leastv) as least)) ->
          if v > mostv then ((k, v), least)
          else if v < leastv then (most, (k, v))
          else (most, least))
        m
        (('H', min_int), ('H', max_int))

    let apply_rules_n max (rules : (char * char, char) Hashtbl.t) w =
      let graph = Hashtbl.create 17 in
      Hashtbl.iter
        (fun (k1, k2) c -> Hashtbl.add graph (k1, k2) ((k1, c), (c, k2)))
        rules;
      let meet = Hashtbl.create 17 in
      let merge =
        CharMap.merge (fun k a b ->
            match (a, b) with
            | Some a, Some b -> Some (a + b)
            | Some a, _ | _, Some a -> Some a
            | None, None -> None)
      in
      let update new_c =
        CharMap.update new_c (function
          | None -> Some 1
          | Some v -> Some (v + 1))
      in
      let aux aux (n, ((k1, k2) as init)) =
        if n >= max then CharMap.empty
        else
          let ((_, new_c) as p1), p2 = Hashtbl.find graph init in
          update new_c @@ merge (aux (n + 1, p1)) (aux (n + 1, p2))
      in
      let apply init = memo_rec meet aux (0, init) in
      let rec fold_w acc i =
        if i >= String.length w - 1 then acc
        else
          fold_w (update w.[i] @@ merge acc (apply (w.[i], w.[i + 1]))) (i + 1)
      in
      let r = update w.[String.length w - 1] @@ fold_w CharMap.empty 0 in
      Format.printf "%d@\n" (Hashtbl.length meet);
      most_least r
  end

  let run () =
    let init, rules = parse "day14.txt" in
    Format.printf "%s@\n%a" init
      (print_seq (fun fmt ((k1, k2), v) ->
           Format.fprintf fmt "%c%c -> %c@\n" k1 k2 v))
      (Hashtbl.to_seq rules);
    let (mostc, mostv), (leastc, leastv) = Part2.apply_rules_n 59 rules init in
    Format.printf "most: %c, %d times@\nleast: %c, %d times@\nres: %d @\n" mostc
      mostv leastc leastv (mostv - leastv)
  ;;

  run ()
end

module Day13 = struct
  type fold = Horizontal of int | Vertical of int

  let parse file =
    foldf
      (fun ((pts, folds) as acc) line ->
        let get_axis line =
          match String.split_on_char '=' line with
          | [ _; axis ] -> int_of_string axis
          | _ -> failwith "input error"
        in
        if String.contains line ',' then
          match String.split_on_char ',' line with
          | [ x; y ] ->
              let x, y = (int_of_string x, int_of_string y) in
              (PtSet.add (x, y) pts, folds)
          | _ -> failwith "input error"
        else if String.contains line 'y' then
          (pts, Horizontal (get_axis line) :: folds)
        else if String.contains line 'x' then
          (pts, Vertical (get_axis line) :: folds)
        else acc)
      (PtSet.empty, []) file

  let fold_paper axis points =
    PtSet.fold
      (fun (x, y) acc ->
        let sym coord axis =
          if coord < axis then coord else axis - (coord - axis)
        in
        let pt =
          match axis with
          | Horizontal axis -> (x, sym y axis)
          | Vertical axis -> (sym x axis, y)
        in
        PtSet.add pt acc)
      points PtSet.empty

  let show points =
    let max_x, max_y =
      PtSet.fold
        (fun (x, y) (max_x, max_y) -> (max x max_x, max y max_y))
        points (0, 0)
    in
    let a = Array.make_matrix (max_y + 1) (max_x + 1) false in
    PtSet.iter (fun (x, y) -> a.(y).(x) <- true) points;
    print_mat (fun v -> if v then "#" else ".") a

  let run () =
    let pts, folds = parse "day13.txt" in

    List.fold_left
      (fun acc fld ->
        let acc = fold_paper fld acc in
        Format.printf "size: %d \n" (PtSet.cardinal acc);
        acc)
      pts (List.rev folds)
    |> show
end

module Day12 = struct
  let parse file =
    let h = Hashtbl.create 17 in
    let assoc vx1 vx2 =
      let prev =
        match Hashtbl.find h vx1 with exception Not_found -> [] | l -> l
      in
      Hashtbl.replace h vx1 (vx2 :: prev)
    in
    foldf
      (fun () line ->
        match String.split_on_char '-' line with
        | [ vx1; vx2 ] ->
            assoc vx1 vx2;
            assoc vx2 vx1
        | _ -> failwith "input error")
      () file;
    h

  let cannot_revisit nb path = int_of_char nb.[0] >= 97 && List.mem nb path

  let explore graph =
    let rec aux paths finished_paths =
      let paths, finished_paths =
        List.fold_left
          (fun (paths, finished_paths) (visited_small, path) ->
            List.fold_left
              (fun ((paths, finished_paths) as acc) nb ->
                if nb = "end" then (paths, (nb :: path) :: finished_paths)
                else if nb = "start" then acc
                else if cannot_revisit nb path then
                  if visited_small then acc
                  else ((true, nb :: path) :: paths, finished_paths)
                else ((visited_small, nb :: path) :: paths, finished_paths))
              (paths, finished_paths)
              (Hashtbl.find graph @@ List.hd path))
          ([], finished_paths) paths
      in
      if paths = [] then finished_paths else aux paths finished_paths
    in
    aux [ (false, [ "start" ]) ] []

  let run () =
    let graph = parse "day12.txt" in
    Hashtbl.iter
      (fun k v ->
        Format.printf "%s " k;
        printl (fun x -> x) v;
        Format.printf "")
      graph;
    let paths = List.rev_map List.rev @@ explore graph in
    List.length paths |> Format.printf "%d\n"
  ;;

  run ()
end

module Day11 = struct
  let visit_neighbours acc board (x, y) f =
    let visit f x y acc =
      match board.(y).(x) with
      | exception Invalid_argument _ -> acc
      | v -> f acc (x, y) v
    in
    visit f (x + 1) y acc
    |> visit f (x - 1) y
    |> visit f x (y - 1)
    |> visit f x (y + 1)
    |> visit f (x + 1) (y + 1)
    |> visit f (x - 1) (y - 1)
    |> visit f (x + 1) (y - 1)
    |> visit f (x - 1) (y + 1)

  let array_foldi f acc a =
    Array.fold_left (fun (i, acc) e -> (i + 1, f i acc e)) (0, acc) a |> snd

  let step n board =
    let increase_flash ((x, y) as pt) (to_flash, flashed) v =
      let v = v + 1 in
      board.(y).(x) <- v;
      if v > 9 then (pt :: to_flash, PtSet.add pt flashed)
      else (to_flash, flashed)
    in
    (* initially increase all octopus and init the to_flash list *)
    let to_flash, flashed =
      array_foldi
        (fun y -> array_foldi (fun x -> increase_flash (x, y)))
        ([], PtSet.empty) board
    in
    (* check all octo to flash and increase their neighbours,
       add them to be flashed aswell
       stop when there is nothing more to flash
    *)
    let rec propagate (to_flash, flashed) =
      match to_flash with
      | [] -> flashed
      | octo :: to_flash ->
          propagate
            (visit_neighbours (to_flash, flashed) board octo
               (fun state nocto nval ->
                 if PtSet.mem nocto (snd state) then state
                 else increase_flash nocto state nval))
    in
    let flashed = propagate (to_flash, flashed) in
    let flashes = PtSet.cardinal flashed in
    if flashes = Array.length board * Array.length board.(0) then
      raise (Exitv n);
    PtSet.iter (fun (x, y) -> board.(y).(x) <- 0) flashed;
    flashes

  let run_steps board max =
    let rec aux n acc =
      let res = step n board in
      if n = max then acc + res else aux (n + 1) (acc + res)
    in
    aux 1 0

  let run () =
    let board = parse_matrix "day11.txt" in
    print_mat string_of_int board;
    try run_steps board 400 |> Format.printf "flashes: %d\n"
    with Exitv n -> Format.printf "Everyone flashed step %d\n" n
end

module Day10 = struct
  let parse file = foldf (fun acc line -> line :: acc) [] file

  let get_infos = function
    | ')' -> ('(', 3)
    | ']' -> ('[', 57)
    | '}' -> ('{', 1197)
    | '>' -> ('<', 25137)
    | _ -> failwith "input error"

  let get_open_score = function
    | '(' -> 1
    | '[' -> 2
    | '{' -> 3
    | '<' -> 4
    | _ -> failwith "input error"

  let checkline l =
    let read_char stack c =
      match c with
      | '(' | '{' | '[' | '<' -> c :: stack
      | c -> (
          match stack with
          | opening :: stack when fst (get_infos c) == opening -> stack
          | _ -> raise (Exitv (snd @@ get_infos c)))
    in
    match String.to_seq l |> Seq.fold_left read_char [] with
    | exception Exitv n -> (Some n, [])
    | stack -> (None, stack)

  let completion_scores l =
    List.fold_left
      (fun acc (_, stack) ->
        List.fold_left (fun acc c -> (acc * 5) + get_open_score c) 0 stack
        :: acc)
      [] l

  let middle_score l =
    let a = Array.of_list @@ List.sort compare l in
    a.(Array.length a / 2)

  let run () =
    let parsed = parse "day10.txt" in
    let errored, unfinished =
      List.map checkline parsed |> List.partition (fun (e, _) -> e <> None)
    in
    let error_score = List.filter_map fst errored |> List.fold_left ( + ) 0 in
    Format.printf "errored: %d\n" error_score;
    Format.printf "completion: %d" (completion_scores unfinished |> middle_score)
end

module Day9 = struct
  module Spot = struct
    type t = int * int

    let compare = compare
  end

  module SpotSet = Set.Make (Spot)

  let parse file =
    foldf
      (fun acc line ->
        let line =
          String.to_seq line
          |> Seq.fold_left (fun acc c -> (Char.code c - 48) :: acc) []
          |> List.rev |> Array.of_list
        in
        line :: acc)
      [] file
    |> List.rev |> Array.of_list

  let visit_neighbours acc f board (x, y) =
    let visit acc f x y =
      match board.(y).(x) with
      | exception Invalid_argument _ -> acc
      | v -> f acc (x, y) v
    in
    let right = visit acc f (x + 1) y in
    let left = visit right f (x - 1) y in
    let down = visit left f x (y - 1) in
    let up = visit down f x (y + 1) in
    up

  let is_risky board spot v =
    visit_neighbours true (fun acc _ nval -> acc && nval > v) board spot

  let explore_basins board risky_spots =
    let rec explore_one acc spot =
      visit_neighbours (SpotSet.add spot acc)
        (fun acc nspot nval ->
          if nval < 9 && not (SpotSet.mem nspot acc) then explore_one acc nspot
          else acc)
        board spot
    in

    let basins_size =
      List.map
        (fun rs -> SpotSet.cardinal @@ explore_one SpotSet.empty rs)
        risky_spots
    in
    match List.sort (fun a b -> -compare a b) basins_size with
    | b1 :: b2 :: b3 :: _ -> b1 * b2 * b3
    | _ -> failwith "wrong input"

  let riskys a =
    let res = ref [] in
    Array.iteri
      (fun y col ->
        Array.iteri
          (fun x v -> if is_risky a (x, y) v then res := (x, y) :: !res)
          col)
      a;
    !res

  let sum_riskys board l =
    List.fold_left (fun acc (x, y) -> board.(y).(x) + 1 + acc) 0 l

  let run () =
    let board = parse "day9.txt" in
    Format.printf "riskys: %d\n" (sum_riskys board @@ riskys board);
    Format.printf "basins: %d" (explore_basins board @@ riskys board)
end

module Day8 = struct
  let parse file =
    foldf
      (fun acc l ->
        match String.split_on_char '|' l with
        | [ signals; output ] ->
            let split_ignore_empty l =
              l |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")
            in
            let signals = split_ignore_empty signals in
            let output = split_ignore_empty output in
            (signals, output) :: acc
        | _ -> failwith "wrong input")
      [] file

  let count_1_4_7_8 =
    List.fold_left
      (fun acc (_, output) ->
        List.fold_left
          (fun acc word ->
            match String.length word with 2 | 3 | 4 | 7 -> acc + 1 | _ -> acc)
          acc output)
      0

  let words_size s =
    match s with
    | 2 -> Some 1
    | 3 -> Some 7
    | 4 -> Some 4
    | 7 -> Some 8
    | _ -> None

  let mk_char_set s =
    Seq.fold_left (fun acc c -> CharSet.add c acc) CharSet.empty
    @@ String.to_seq s

  let compute l =
    List.fold_left
      (fun acc (signals, output) ->
        let words = Array.make 10 CharSet.empty in

        (* fill words with easiests and filter them *)
        let hardest =
          List.filter
            (fun word ->
              let len = String.length word in
              match words_size len with
              | Some v ->
                  words.(v) <- mk_char_set word;
                  false
              | None -> true)
            signals
        in

        let len5, len6 =
          List.partition (fun x -> String.length x = 5) hardest
        in

        (* find assoc for numbers with words of len 6 *)
        List.iter
          (fun w ->
            let w = mk_char_set w in
            if CharSet.subset words.(4) w then words.(9) <- w
            else if CharSet.subset words.(7) w then words.(0) <- w
            else words.(6) <- w)
          len6;

        (* find assoc for numbers with words of len 6 *)
        List.iter
          (fun w ->
            let w = mk_char_set w in
            if CharSet.subset w words.(6) then words.(5) <- w
            else if CharSet.subset words.(7) w then words.(3) <- w
            else words.(2) <- w)
          len5;

        (* convert array to map word -> number *)
        let map =
          Array.fold_left
            (fun (i, map) w -> (i + 1, CharSetMap.add w i map))
            (0, CharSetMap.empty) words
          |> snd
        in

        (* find numbers depending on output and concat them *)
        let res =
          List.fold_left
            (fun acc ow ->
              acc ^ string_of_int (CharSetMap.find (mk_char_set ow) map))
            "" output
          |> int_of_string
        in

        res + acc)
      0 l

  let run () =
    let r = parse "day8.txt" in
    r |> count_1_4_7_8 |> print_int;
    print_endline "";
    compute r |> print_int;
    print_endline "";
    ()

  let () = run ()
end

module Day7 = struct
  let parse file =
    foldf
      (fun acc l ->
        String.split_on_char ',' l |> List.map int_of_string
        |> List.sort compare |> Array.of_list)
      [||] file

  let compute_part1 file =
    let positions = parse file in
    let len = Array.length positions in
    let median =
      if len mod 2 = 0 then
        (positions.((len / 2) - 1) + positions.(len / 2)) / 2
      else positions.((len / 2) + 1)
    in
    Array.fold_left (fun acc p -> abs (p - median) + acc) 0 positions

  let intsum n = n * (n + 1) / 2

  let compute_part2 file =
    let positions = parse file in
    let mean =
      float_of_int (Array.fold_left ( + ) 0 positions)
      /. float_of_int (Array.length positions)
    in
    Array.fold_left
      (fun acc p ->
        let r = intsum (abs (p - int_of_float mean)) in
        r + acc)
      0 positions

  let run () =
    compute_part1 "day7.txt" |> Format.printf "part1: %d\n";
    compute_part2 "day7.txt" |> Format.printf "part2: %d\n"
end

module Day6 = struct
  let parse file =
    foldf
      (fun acc l ->
        String.split_on_char ',' l |> List.map int_of_string
        |> List.sort compare)
      [] file

  let compute max_round init =
    (* Rounds are identified by their cycle id from 0 to 6.
       Each round, we trigger the fishes of the current cycle id.
       The array q contains the number of fishes for a given cycle id.
       The array fresh_q delays the "fresh fishes" two turns before
       they get included to the main queue. It uses a cycle of 2 ids.
    *)
    let q = Array.make 7 0 in
    let fresh_q = Array.make 2 0 in
    List.iter (fun e -> q.(e) <- q.(e) + 1) init;
    let rec round n =
      let rec play_round cycle cycle_fresh =
        let current_q = q.(cycle) in
        (* add fresh fishes to the main queue *)
        q.(cycle) <- q.(cycle) + fresh_q.(cycle_fresh);
        (* generate fresh fishes *)
        fresh_q.(cycle_fresh) <- current_q
      in
      if n >= max_round then ()
      else (
        play_round (n mod 7) (n mod 2);
        round (n + 1))
    in
    round 0;
    (* please stay less than maxint *)
    Array.fold_left ( + ) 0 q + Array.fold_left ( + ) 0 fresh_q

  let run () =
    let init = parse "day6.txt" in
    compute 256 init |> print_int
end

module Day5 = struct
  (** add points by moving from p1 to p2
      and return the number of new collision *)
  let rec add_points h acc p1 p2 =
    let colision, acc =
      match Hashtbl.find_opt h p1 with
      (* already in colision *)
      | Some true -> (false, acc)
      (* still not in colision *)
      | Some false ->
          Hashtbl.replace h p1 true;
          (true, acc + 1)
      | None ->
          Hashtbl.add h p1 false;
          (false, acc)
    in
    if p1 = p2 then acc
    else
      let (x1, y1), (x2, y2) = (p1, p2) in
      let move_coord c1 c2 =
        if c1 < c2 then c1 + 1 else if c1 = c2 then c1 else c1 - 1
      in
      let y1 = move_coord y1 y2 in
      let x1 = move_coord x1 x2 in
      add_points h acc (x1, y1) p2

  (** Parse and add points to env *)
  let compute file =
    let h = Hashtbl.create 17 in
    foldf
      (fun acc line ->
        match
          Scanf.sscanf line "%d,%d -> %d,%d" (fun x1 y1 x2 y2 ->
              ((x1, y1), (x2, y2)))
        with
        | p1, p2 -> add_points h 0 p1 p2 + acc
        | exception Scanf.Scan_failure _ -> failwith "scan failure")
      0 file

  let run () = compute "day5.txt" |> print_int
end

module Day4 = struct
  let parse =
    foldf
      (fun ((cur_mat, matrixes, draw) as acc) line ->
        match line with
        | "" ->
            if cur_mat == [] then acc
            else ([], (Array.of_list @@ List.rev cur_mat) :: matrixes, draw)
        | s when String.contains s ',' ->
            let draw = String.split_on_char ',' s |> List.map int_of_string in
            (cur_mat, matrixes, draw)
        | s ->
            let splitted =
              String.split_on_char ' ' s
              |> List.filter_map (fun x ->
                     if x <> "" then Some (int_of_string x, false) else None)
            in
            let mat_line = splitted |> Array.of_list in
            (mat_line :: cur_mat, matrixes, draw))
      ([], [], [])

  let build_matrixes cur_mat matrixes =
    let h = Hashtbl.create 57 in
    let matrixes =
      match cur_mat with
      | [] -> matrixes
      | last -> Array.of_list last :: matrixes
    in
    let matrixes = Array.of_list matrixes in
    Array.iteri
      (fun i mat ->
        Array.iteri
          (fun y col ->
            Array.iteri
              (fun x (v, _) ->
                let locs =
                  match Hashtbl.find h v with
                  | exception Not_found -> []
                  | locs -> locs
                in
                Hashtbl.add h v ((i, x, y) :: locs))
              col)
          mat)
      matrixes;
    (h, matrixes)

  let sum_unchecked mat =
    Array.fold_left
      (fun acc col ->
        Array.fold_left
          (fun acc (v, checked) -> if not checked then acc + v else acc)
          acc col)
      0 mat

  let check_col_line mat x y lastval =
    let check_rows () =
      let width = Array.length mat in
      let rec aux y all_checked =
        if y >= width then all_checked
        else
          let checked = snd mat.(y).(x) in
          checked && aux (y + 1) true
      in
      aux 0 true
    in
    let col_checked = Array.for_all (fun (_, checked) -> checked) mat.(y) in
    if col_checked || check_rows () then Some (lastval * sum_unchecked mat)
    else None

  let compute_bingo h draw matrixes =
    let wins = ref IntSet.empty in
    try
      List.iter
        (fun v ->
          let locs = Hashtbl.find h v in
          List.iter
            (fun (mat, x, y) ->
              matrixes.(mat).(y).(x) <- (v, true);
              match check_col_line matrixes.(mat) x y v with
              | None -> ()
              | Some v ->
                  wins := IntSet.add mat !wins;
                  if IntSet.cardinal !wins = Array.length matrixes then
                    raise (Exitv v)
                  else ())
            locs)
        draw;
      failwith "bingo failed"
    with Exitv v -> v

  let run () =
    print_endline "Day 4";
    let cur_mat, matrixes, draw = parse "day4.txt" in
    let h, matrixes = build_matrixes cur_mat matrixes in
    compute_bingo h draw matrixes |> print_int;
    print_endline ""
end

module Day3 = struct
  let to_array str =
    Array.init (String.length str) (fun i ->
        if String.get str i == '0' then 0 else 1)

  let from_base_2 arr =
    Array.fold_right
      (fun c (acc, mult) ->
        match c with
        | 0 -> (acc, mult * 2)
        | 1 -> (acc + mult, mult * 2)
        | _ -> failwith "input error")
      arr (0, 1)
    |> fst

  let inverse =
    Array.map (function 1 -> 0 | 0 -> 1 | _ -> failwith "input error")

  let compute_power_consumption file =
    let resize line arr =
      let diff = String.length line - Array.length arr in
      Array.concat [ arr; Array.make diff 0 ]
    in
    (* sum number of 0 *)
    let nb_lines, nb_zero_arr =
      foldf
        (fun (nb_lines, nb_zero_arr) line ->
          let nb_zero_arr =
            if String.length line > Array.length nb_zero_arr then
              resize line nb_zero_arr
            else nb_zero_arr
          in
          String.iteri
            (fun i c ->
              if c == '0' then Array.(set nb_zero_arr i (get nb_zero_arr i + 1)))
            line;
          (nb_lines + 1, nb_zero_arr))
        (0, Array.make 0 0)
        file
    in
    (* replace by most used *)
    let gamma_arr =
      Array.mapi
        (fun i nb_zero -> if nb_zero > nb_lines / 2 then 0 else 1)
        nb_zero_arr
    in
    let epsilon = from_base_2 @@ inverse gamma_arr in
    let gamma = from_base_2 gamma_arr in
    gamma * epsilon

  let compute_life_support_rating file =
    let input = foldf (fun acc line -> line :: acc) [] file in
    let line_len =
      match input with h :: _ -> String.length h | _ -> failwith "empty input"
    in
    let rec split_on_bit bitr ((with0, nb0, with1, nb1) as acc) l =
      match l with
      | num :: t ->
          let res =
            if String.get num bitr == '0' then
              (num :: with0, nb0 + 1, with1, nb1)
            else (with0, nb0, num :: with1, nb1 + 1)
          in
          split_on_bit bitr res t
      | [] -> acc
    in
    let most_common (with0, nb0, with1, nb1) =
      if nb0 > nb1 then with0 else with1
    in
    let least_common (with0, nb0, with1, nb1) =
      if nb0 <= nb1 then with0 else with1
    in
    let rec apply_filters choice bitr l =
      if bitr >= line_len || match l with [ _ ] -> true | _ -> false then l
      else
        let splitted = split_on_bit bitr ([], 0, [], 0) l in
        apply_filters choice (bitr + 1) @@ choice splitted
    in
    let oxy_gen = apply_filters most_common 0 input in
    let co2_scrub = apply_filters least_common 0 input in
    let oxy_gen, co2_scrub =
      match (oxy_gen, co2_scrub) with
      | [ oxy_gen ], [ co2_scrub ] ->
          let oxy_gen, co2_scrub = (to_array oxy_gen, to_array co2_scrub) in
          (from_base_2 oxy_gen, from_base_2 co2_scrub)
      | _ -> failwith "results should be singleton"
    in
    oxy_gen * co2_scrub

  let run () =
    print_endline "Day 3";
    Format.printf "power_consum: %d\n" @@ compute_power_consumption "day3.txt";
    Format.printf "life_support: %d\n" @@ compute_life_support_rating "day3.txt"
end

module Day2 = struct
  type location = { h_pos : int; depth : int; aim : int }

  type instr = Fwd | Down | Up

  let to_instr line =
    let instr, n =
      match line with
      | [ "forward"; n ] -> (Fwd, n)
      | [ "down"; n ] -> (Down, n)
      | [ "up"; n ] -> (Up, n)
      | _ -> failwith "instruction error"
    in
    (instr, int_of_string n)

  let compute_location file =
    foldf
      (fun ({ h_pos; depth; aim } as loc) l ->
        match to_instr @@ String.split_on_char ' ' l with
        | Fwd, n -> { loc with h_pos = h_pos + n; depth = depth + (n * aim) }
        | Down, n -> { loc with aim = aim + n }
        | Up, n -> { loc with aim = aim - n })
      { h_pos = 0; depth = 0; aim = 0 }
      file

  let run () =
    print_endline "Day 2";
    let { h_pos; depth } = compute_location "day2.txt" in
    Format.printf "Location: %d, %d\n" h_pos depth;
    Format.printf "Result: %d\n" (h_pos * depth)
end

module Day1 = struct
  let compute_increases l =
    List.fold_left
      (fun (acc, prev) e -> ((if e > prev then acc + 1 else acc), e))
      (-1, 0) l
    |> fst

  let compute_3sum_increases l =
    let init_sum, short_l =
      match l with
      | e1 :: e2 :: e3 :: l' -> (e1 + e2 + e3, l')
      | _ -> failwith "list too short"
    in
    let rec loop result (short_sum, sum) short_l l =
      match (short_l, l) with
      | [], _ -> result
      | short_h :: short_t, h :: t ->
          let short_sum = short_h + short_sum in
          let sum = h + sum in
          loop ((short_sum - sum) :: result) (short_sum, sum) short_t t
      | _ -> failwith "pouet pouet cannot happen"
    in
    loop [] (init_sum, 0) short_l l |> List.rev

  let run () =
    print_endline "Day 1";
    let l =
      foldf (fun acc x -> int_of_string x :: acc) [] "day1.txt" |> List.rev
    in
    compute_3sum_increases l |> compute_increases |> print_int
end
