open Graphics
open Buttons
open Text_boxes

let main () =
  open_graph " 640x480";
  add_button {name = "Hello"; pos = (20,20,50,20); action = (fun () -> draw_string "Hello World!");};
  add_button {name = "Quit"; pos = (20,60,50,20); action = (fun () -> exit 0);};

  add_text_field {text = ""; text_len = 5; pos = (80,20,50,20);};

  draw_button 0;
  draw_button 1;
  draw_textbox 0;

  while true do
    let e = wait_next_event [Button_down] in
    match e with {mouse_x = x; mouse_y = y; _} ->
      if (in_box x y) then(
        let b = get_from_pos x y in
        request_action b;
      );
      if (in_field x y) then(
        let txt_b = get_txt_from_pos x y in
        let msg = input_text txt_b in
        moveto 200 200;
        draw_string msg;
      );
  done;;

main ();