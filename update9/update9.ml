open Unix

let n = ref 16
let nmax = 16
let p_ag = ref 0.3
let p_cheat = ref 0.03

let sample = ref 100

let ff = ref Format.std_formatter

let time = ref 0
let moves = ref 0
let moved = ref false
type status = { signals : bool array; mutable agents : int }

module Cset = Set.Make(struct type t = (int*int) let compare=compare end) 

let t = Array.make_matrix nmax nmax {signals = [||] ; agents = 0}
let neigh = Array.make_matrix nmax nmax Cset.empty

let add_agents i j nb = t.(i).(j).agents <-  nb + t.(i).(j).agents




let get_neighbors i j = 
  let rec aux sx sy acc =
    if sy = 2 then acc
    else if sx = 2 
    then aux (-1) (sy+1) acc
    else if (sx=0)&&(sy=0) then aux 1 0 acc
    else let (a,b) = (i+sx, j+sy) in
	 if (a>=0)&&(a<(!n))&&(b>=0)&&(b<(!n))
	 then aux (sx+1) sy (Cset.add (a,b) acc)
	 else aux (sx+1) sy acc
  in
  aux (-1) (-1) Cset.empty


let init () = 
  Random.self_init ();
  for i = 0 to (!n) -1 do
    for j = 0 to (!n) - 1 do
      t.(i).(j) <- {signals = Array.make 8 false; agents = 0};
      neigh.(i).(j)<-get_neighbors i j
    done
  done;
  let rec place_agent () = 
    let i,j = (Random.int (!n), Random.int (!n)) in
    if t.(i).(j).agents = 0 then add_agents i j 1
    else place_agent () in
  for a = 0 to (!n) - 1 do
    place_agent ()
  done


let dir (i1,j1) (i2,j2) = 
  match (i1-i2), (j1-j2) with
  |(-1, -1)->0
  |(-1, 0) -> 4
  |(-1 ,1) -> 5
  |(0, -1) -> 1
  |(0,1) -> 6
  |(1,-1) -> 2
  |(1,0) -> 3
  |(1,1)-> 7
  |_-> failwith "dir : not neighbours"



(*  0 1 2
    4 X 3
    5 6 7  : dir a b = 7 - (dir b a)  *)


let safestay i j = 
  let rec aux k = 
    k >= 8 || (not t.(i).(j).signals.(k) && aux (k+1)) in
  t.(i).(j).agents <= 1 && aux 0

let safego i j d =
    let rec aux k = 
    k >= 8 || ((not t.(i).(j).signals.(k) ||k=d) && aux (k+1)) in
  t.(i).(j).agents = 0 && aux 0

let danger i j d = 
  let rec aux k = 
    if k >=8 then 0 else
     (if (k <> d && t.(i).(j).signals.(k)) then 1 else 0)+(aux (k+1))
  in
  if t.(i).(j).agents > 0 then max_int else aux 0

(*
let choose_update () = 
  match Random.int 8 with
  |4 -> let i = Random.int (n-1) and j = Random.int n in
	((i,j),(i+1,j))
  |1 -> let i = Random.int n and j = Random.int (n-1) in
	((i,j),(i,j+1))
  |0-> let i = Random.int (n-1) and j = Random.int (n-1) in
       ((i,j),(i+1,j+1))
  |2 -> let i = 1 + Random.int (n-1) and j = Random.int (n-1) in
	((i,j), (i-1, j+1))
  |3 -> let i = 1 + Random.int (n-1) and j = Random.int n in
	((i,j), (i-1, j))
  |5 -> let i = Random.int (n-1) and j = 1 + Random.int (n-1) in
	((i,j), (i+1, j-1))
  |6 -> let i = Random.int n and j = 1 + Random.int (n-1) in 
	((i,j),(i,j-1))
  |_ -> let i = 1+ Random.int (n-1) and j = 1 + Random.int (n-1) in
	((i,j),(i-1,j-1))
*)
(*
let choose_update () = 
  let n = !n in
  let m = (4*n-2)*(n-1) in
  let r = Random.int m in
  let a = r/(n-1) and b = r mod (n-1) in
  if a<n then ((b,a),(b+1,a))
  else if a < 2*n then ((a-n, b), (a-n, b+1))
  else if a < 3*n -1 then ((a-2*n, b), (a +1 -2*n, b+1))
  else ((a+1-3*n,b+1), (a+2-3*n,b))
*)

let choose_update () = 
  (Random.int (!n), Random.int (!n))

let attempt_move (i1,j1) (i2,j2) = 
  let p_cheat = !p_cheat in
  let try_cheat = Random.float 1. in
  let d = dir (i2, j2) (i1,j1) in
  if (not (safestay i1 j1))
    &&((safego i2 j2 d) || (try_cheat<=p_cheat) (*||(danger i2 j2 d <
						     danger i1 j1 d) *))
    &&(t.(i1).(j1).agents > 0)(*&&(t.(i2).(j2).agents = 0)*)
  then 
    ( moved := true;
     (* Format.printf "move from %d,%d to %d,%d at time %d " i1 j1 i2 j2
	!time;
      if try_ag <= p_ag then Format.printf "agitated ";
      if try_cheat <= p_cheat then Format.printf "cheated";
      Format.printf"@.";*)
      incr moves;
      true)
      
  else false
  


let update (i,j) = 
  let s = neigh.(i).(j) in
  let update_sig (i,j) (i1,j1) = 
    let d = dir (i,j) (i1,j1) in
    t.(i).(j).signals.(d) <- (t.(i1).(j1).signals.(d) ||
				t.(i1).(j1).agents > 0) 
  in
  Cset.iter (fun (i1,j1) -> update_sig (i,j) (i1,j1)) s;
  let min_danger = Cset.fold (fun (i1,j1) m -> let d = danger i1 j1
						 (dir (i1,j1) (i,j))
					       in min m d) s max_int in
  let smin = if (Random.float 1.) < !p_ag then s else Cset.filter (fun (i1,j1) -> 
    danger i1 j1 (dir (i1,j1) (i,j)) = min_danger) s in
  let c = Cset.cardinal smin in
  let (i1,j1) = (Array.of_list (Cset.elements smin)).(Random.int c) in
  if attempt_move (i,j) (i1,j1)
    then (add_agents i j (-1); add_agents i1 j1 1)

 (* if !moved then Cset.iter (fun (i1,j1) -> update_sig (i,j) (i1,j1)) s*)
    
    
(*
let update ((i1,j1), (i2,j2))  =
  let d = dir (i1,j1) (i2,j2) in
  begin match ( attempt_move (i1,j1) (i2,j2), attempt_move (i2,j2) (i1,j1))
  with
  |false, false |true, true -> ()
  |true, false ->
    add_agents i1 j1 (-1);    add_agents i2 j2 1
  |false, true ->
    add_agents i1 j1 1;  add_agents i2 j2 (-1)
  end;
  t.(i1).(j1).signals.(d) <- (t.(i2).(j2).signals.(d) ||
				(t.(i2).(j2).agents > 0));
  t.(i2).(j2).signals.(7-d) <- (t.(i1).(j1).signals.(7-d) ||
				  (t.(i1).(j1).agents > 0))
    
*)  
    
let print_board ()= 
  let n = !n in
  Format.printf "Time %d, moves %d@." !time !moves;
  for i = 0 to n-1 do
    for j = 0 to n - 1 do
      Format.printf "%d " t.(i).(j).agents;
    done;
    Format.printf "@."
  done


    
let print_safe () =
  let n = !n in
 Format.printf "Safe spots@.";
  for i = 0 to n -1 do
    for j = 0 to n - 1 do
      Format.printf "%d " (if safestay i j then 1 else 0);
    done;
    Format.printf "@."
  done

let solved () = 
  let n = !n in 
  let rec vert j ind seen = 
    if ind >= n then true
    else match t.(ind).(j).agents with
    |1 -> if seen then false else vert j (ind+1) true
    |0 -> vert j (ind + 1) seen
    |_-> false
  in
  let rec hor i ind seen = 
    if ind >= n then true
    else match t.(i).(ind).agents with
    |1 -> if seen then false else hor i (ind+1) true
    |0 -> hor i (ind+1) seen
    |_-> false
  in
  let rec diag i j seen up = 
    if (i>=n)||(j>=n)||(i<0) then true
    else begin
      let (i2,j2) = ((if up then i-1 else i+1), j+1) in
      match t.(i).(j).agents with
      |1-> if seen then false else diag i2 j2 true up
      |0 -> diag i2 j2 seen up
      |_-> false
    end
  in
  let rep = ref true in
  let k = ref 0 in
  while (!rep && (!k<n)) do
    rep:=(vert !k 0 false)&&(hor !k 0 false)&&(diag (n-1) !k false
  true)&&(diag !k 0 false true)&&(diag 0 !k false false)&&(diag !k 0
  false false);
    incr k;
  done;
  !rep

let exp () = 
  time := 0;
  moves := 0;
  init ();
  let fini = ref false in
  while (not !fini) do
    moved := false;
    update (choose_update ());
    (*if (!time mod 1000000 = 0) then (print_board (); Unix.sleep 1);*)
    incr time;
    if (!time mod 1000 = 0) then fini := (solved ());
    
  done;
  (*print_board ();*)
  !time, !moves
  
 

let run tot = 
let rec run i tot acct accm =
  if ((tot-i)*5 mod tot = 0)||i+1=tot then Format.printf "%d done...@." (tot - i); 
  if i <= 0 then 
    Format.printf "With size %d, p_cheat %f :  average time %d,
average number of moves %d over %d runs.@." !n !p_cheat (acct/tot)
      (accm/tot) tot
  else let t,m = exp () in
       (*Format.fprintf !ff "%d, %d" !times !moves;*)
       run (i-1) tot (acct + t) (accm + m)
in
run tot tot 0 0

(*
let () = 
 let l = [(*0.001; 0.005; 0.008; 0.009;*) 0.01;(* 0.011; 0.012; 0.013;
    0.014;*) 0.015; 0.02;0.025; 0.03  ] in 
  (* let l = [0.05; 0.075; 0.1; 0.15; 0.2;0.25; 0.3; 0.35] in*)
  let rec aux l = 
    match l with [] -> ()
    |p::t -> p_cheat:=p; run 50; aux t
  in
  aux l
*)

(*let _ = exp ()*)

let options = ["-p", Arg.Float (fun f -> p_cheat := f), "sets p_cheat";
	       "-n", Arg.Set_int n, "sets board size";
	       "-sample", Arg.Set_int sample, "sets sample size"]
(*
let () = 
  Arg.parse options (fun s -> ()) "";
  for s = 4 to 9 do
    n:=s;
    let fd = Unix.openfile (string_of_int s) [O_WRONLY; O_CREAT] 0o640
  in
    ff:=Format.formatter_of_out_channel (out_channel_of_descr fd);
    for i = 1 to !sample do
      let (t,m) = exp () in
      Format.fprintf !ff "%d, %d@." t m;
    done
  done
*)

let _ = n:=8; run 100






