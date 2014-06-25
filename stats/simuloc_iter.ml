let n = ref 6

let p_ag = 0.
let p_cheat = ref 0.01

let time = ref 0
let moves = ref 0
let moved = ref false
type status = { signals : bool array; mutable agents : int }

let t = Array.make_matrix !n !n {signals = [||] ; agents = 0}

let add_agents i j nb = t.(i).(j).agents <-  nb + t.(i).(j).agents

let init () = 
  Random.self_init ();
  for i = 0 to (!n) -1 do
    for j = 0 to (!n) - 1 do
      t.(i).(j) <- {signals = Array.make 8 false; agents = 0}
    done
  done;
  for a = 0 to (!n) - 1 do
    add_agents (Random.int (!n)) (Random.int (!n)) 1
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
  t.(i).(j).agents+(aux 0)

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

let choose_update () = 
  let n = !n in
  let m = (4*n-2)*(n-1) in
  let r = Random.int m in
  let a = r/(n-1) and b = r mod (n-1) in
  if a<n then ((b,a),(b+1,a))
  else if a < 2*n then ((a-n, b), (a-n, b+1))
  else if a < 3*n -1 then ((a-2*n, b), (a +1 -2*n, b+1))
  else ((a+1-3*n,b+1), (a+2-3*n,b))


let attempt_move (i1,j1) (i2,j2) = 
  let p_cheat = !p_cheat in
  let try_ag = Random.float 1. and try_cheat = Random.float 1. in
  let d = dir (i2, j2) (i1,j1) in
  if ((not (safestay i1 j1)) || (try_ag <= p_ag))
    &&((safego i2 j2 d) || (try_cheat<=p_cheat))
    &&(t.(i1).(j1).agents > 0)
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
  !time, !moves
  (*print_board ()*)

let run tot = 
let rec run i tot acct accm =
 (* if ((tot-i)*10 mod tot = 0)||i+1=tot then Format.printf "%d done...@." (tot - i); *)
  if i <= 0 then 
    Format.printf "With size %d, p_cheat %f :  average time %d,
average number of moves %d over %d runs.@." !n !p_cheat (acct/tot)
      (accm/tot) tot
  else let t,m = exp () in
       run (i-1) tot (acct + t) (accm + m)
in
run tot tot 0 0

let () = 
  let l = [0.001; 0.005; 0.008; 0.01; 0.012; 0.015; 0.02] in
  let rec aux l = 
    match l with [] -> ()
    |p::t -> p_cheat:=p; run 50; aux t
  in
  aux l
