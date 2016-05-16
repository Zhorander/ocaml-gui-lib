open Graphics

type text_field = {
  mutable text : string;
  mutable text_len : int;
  pos : int * int * int * int;
}

let text_list = ref [];;

let add_text_field (box:text_field) =
  text_list := box :: !text_list;;

let draw_textbox n =
  let tb = List.nth !text_list n in
  let (x,y,w,h) = tb.pos in
  draw_rect x y w h;;

let get_txt_from_pos x y =
  let l = List.length !text_list in
  let ret = ref (List.nth !text_list 0) in
  for i = 0 to l - 1 do
    let b = List.nth !text_list i in
    let (b_x,b_y,w,h) = b.pos in
    if x >= b_x && x <= (b_x + w) && y >= b_y && y <= (b_y + h) then ret := b;
  done;
  !ret;;

let in_field x y =
  let l = List.length !text_list in
  let ret = ref false in
  for i = 0 to l - 1 do
    let b = List.nth !text_list i in
    let (b_x,b_y,w,h) = b.pos in
    if x >= b_x && x <= (b_x + w) && y >= b_y && y <= (b_y + h) then ret := true;
  done;
  !ret;;

let get_next_char () =
  let e = wait_next_event [Key_pressed] in
  e.key;;

let draw_array a e = 
  for i = 0 to e do
    draw_char a.(i);
  done;;

let string_of_char c = 
  Printf.sprintf "%c" c;;

let input_text (field:text_field) =
  let max = field.text_len in
  let word = Array.make (max + 1) ' ' in
  let ch = ref 'a' in
  let current = ref 0 in
  let txt = ref "" in
  let (x,y,w,h) = field.pos in
  while !ch <> '\r' do
    ch := get_next_char ();
    if !ch == '\b' && !current >= 0 then(
      word.(!current) <- ' ';
      if !current != 0 then current := !current - 1;
    );
    if !ch <> '\r' && !ch <> '\b' && !current < (max) then(
      word.(!current) <- !ch;
      current := !current + 1;
    );
    moveto (x + 2) (y + 2);
    set_color white;
    fill_rect (x + 2) (y + 2) (w - 4) (h - 4);
    set_color black;
    draw_array word !current;
    (* for i = 0 to (max - !current - 1) do
      draw_string " ";
    done;  *)
  done;
  for i = 0 to !current do
    txt := !txt ^ (string_of_char word.(i));
  done;
  field.text <- !txt;
!txt;;