open Graphics

type button = {
  name : string; (*button label*)
  pos : int * int * int * int; (*x y w h*)
  action : unit -> unit; (*function to execute when button pressed*)
};;

let button_list = ref [];;

let add_button (new_button:button) =
  button_list := new_button :: !button_list;;

let request_action bttn =
  match bttn with {action = a; _} ->
    a ();;

let draw_button num =
  set_color black;
  let b = List.nth !button_list num in
  match b with {name = n; pos = (x,y,w,h); _} ->
    moveto (x + 2) (y + 2);
    draw_string n;
    draw_rect x y w h;;

let in_box x y =
  let l = List.length !button_list in
  let ret = ref false in
  for i = 0 to (l - 1) do
    let b = List.nth !button_list i in
    match b with 
    | {pos = (b_x,b_y,b_w,b_h); _} -> 
      if (x >= b_x && x <= (b_x + b_w) && y >= b_y && y <= (b_y + b_h)) then ret := true;
  done;
!ret;;

let get_from_pos x y =
  let b = ref (List.nth !button_list 0) in
  let l = List.length !button_list in
  for i = 0 to (l - 1) do
    let bttn = List.nth !button_list i in
    match bttn with 
    | {pos = (b_x,b_y,b_w,b_h); _} -> 
      if (x >= b_x && x <= (b_x + b_w) && y >= b_y && y <= (b_y + b_h)) then b := bttn;
  done;
!b;;