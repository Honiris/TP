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

let draw_line list size col =
  let rec dl l1 i =
    match l1 with
      | []   -> ()
      | e::q -> draw_cell (col, size * i + size-3) size (cell_color (nth_int i list)); dl q (i+1)
  in
  dl list 0 ;;

let rules0_line board x =
  let rec r0l l1 y =
    match l1 with
      | []   -> []
      | e::q -> (rules0 e (count_neighbours (x, y) board (0, 0)))::r0l q (y-1)
  in
  r0l (nth_list x board) (length (nth_list x board)-1) ;;


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
  set_color color;
  fill_rect (x+2) (y+2) (size-1) (size-1);
  set_color grey;
  draw_rect (x+1) (y+1) size size ;;

let draw_board board size =
  begin
  clear_graph ();
  let rec db lg =
    match lg with
      | 0 -> draw_line (nth_list 0 board) size (size-3)
      | n -> draw_line (nth_list n board) size (size*n+size-3); db (lg-1)
  in
  db (length board)
  end ;;

(************************************************************)
(*                     Game of life                         *)
(************************************************************)

let rules0 cell near =
  if cell = 0
  then
    if near = 3
    then
      1
    else
      0
  else
    if near = 2 || near = 3
    then
      1
    else
      0 ;;

let rec seed_life board size nb_cell =
  match nb_cell with
    | 0 -> board
    | n -> seed_life (put_cell 1 (Random.int (size-1), Random.int (size-1)) board) size (n-1) ;;

let new_board size nb_cell =
  let mat = gen_board (size, size) 0 in
    seed_life mat size nb_cell ;;

let next_generation board =
  let rec nr x =
    match x with
      | 0 -> []
      | n -> (rules0_line board n)::nr (x-1) 
  in
  nr (length board - 1) ;;

let rec game board n =
  match n with
    | 0 -> draw_board board cell_size 
    | n -> draw_board board cell_size; game (next_generation board) (n-1) ;;
