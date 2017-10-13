(************************************************************)
(*                     Game of life                         *)
(************************************************************)

(* graphics *)

#load "graphics.cma" ;;
open Graphics ;;

let open_window size =
  open_graph(" " ^ string_of_int size ^ "x" ^ string_of_int (size+20));;

let grey = rgb 127 127 127 ;;

let cell_color = function
  | 0 -> white
  | _ -> black ;;

let cell_size = 10 ;;

(* original game of life definitions *)

let new_cell = 1 ;;

let empty = 0 ;;

let is_alive cell = cell <> empty ;;


(*******************   FROM TP 3 *********************)
(*   insert here needed simple functions on lists    *)

let rec length liste =
   match liste with
     | []   -> 0
     | e::q -> 1 + length q ;;

let rec count liste x a b =
  match liste with
    | []   -> 0
    | e::q -> (if !(is_alive e) then 0 else 1) + count q x ;;

let nth i l =
  if i < 0 || l = []
  then
    invalid_arg "nth: index has to be positive and list must not be empty"
  else
    let rec nt k li =
      match (k, li) with
	| (_, [])   -> failwith "nth: element not found"
	| (0, e::q) -> e
	| (x, e::q) -> nt (x-1) q
    in
    nt i l ;;

let search_pos x l =
  let rec sp x l i =
    match l with
      | []   -> failwith "search_pos: not found "
      | e::q -> if x = e then i else sp x q (i+1)
  in
  sp x l 0 ;;

let rec init_list n x =
  match n with
    | 0 -> []
    | y -> x::init_list (y-1) x ;;

let rec replace list i x =
  match (list, i) with
    | ([], _)   -> []
    | (e::q, n) -> if n = 0 then
	             x::replace q (n-1) x
                   else
	             e::replace q (n-1) x ;;
	

(*******************   Toolbox *********************)
(*             list list functions                 *)

(* gen_board *)

let rec gen_board (l, c) x =
  match l with
    | 0 -> []
    | n -> (init_list c x)::gen_board (n-1, c) x ;; 

(* get_cell *)

let get_cell (x, y) board =
  nth y (nth x board) ;;

(* put_cell *)

let rec put_cell xx (x, y) board =
  replace board x (replace (nth x board) y xx) ;;

(* count_neighbours *)

let rec count_neighbours (x, y) board (l, c) =
  match l with
    | 0

(************************************************************)
(*                  graphics                                *)
(*        from the board to the graphic window              *)



(************************************************************)
(*                     Game of life                         *)
(************************************************************)



