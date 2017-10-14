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

let draw_line list size col =
  let rec dl l1 si i =
    match list with
      | []   -> ()
      | e::q -> draw_cell (col, size * i) cell_size (cell_color (nth_int i list)); draw_line q size col
  in
  dl list size 0 ;;


let rec nth_list i list =
  match list with
    | []   -> []
    | e::q -> if i = 0 then e else nth_list (i-1) q ;;

let rec nth_int i list =
  match list with
    | []   -> 0
    | e::q -> if i = 0 then e else nth_int (i-1) q ;;

let rec length liste =
   match liste with
     | []   -> 0
     | e::q -> 1 + length q ;;

let rec count_line liste a b =
  match b with
    | n when n = a -> nth_int a liste
    | n            -> nth_int n liste + count_line liste a (n-1) ;;


let does_exist board (x, y) =
  if x >= length board || x <0 || y < 0 || y >= length (nth_list x board)
  then
    0
  else
    1 ;;

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
  nth_int y (nth_list x board) ;;

(* put_cell *)

let rec put_cell xx (x, y) board =
  if does_exist board (x, y) = 0
  then
    board
  else
    replace board x (replace (nth_list x board) y xx) ;;

(* count_neighbours *)

let count_neighbours (x, y) board (l, c) =
  let rec ctn a b =
    match b with
      | n when n = a -> does_exist board (x, y) * count_line (nth_list a (board)) (y-1) (y+1) - 1
      | n            -> does_exist board (x, y) * count_line (nth_list n (board)) (y-1) (y+l) + ctn a (n-1)
  in
  ctn (x-1) (x+1) ;;

(************************************************************)
(*                  graphics                                *)
(*        from the board to the graphic window              *)

let draw_cell (x, y) size color =
  set_color grey;
  draw_rect (x+1) (y+1) size size;
  set_color color; 
  fill_rect (x+2) (y+2) (size-2) (size-2) ;;

let draw_board board size =



(************************************************************)
(*                     Game of life                         *)
(************************************************************)
