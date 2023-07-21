open Unix
open Bytes

let raw_mode config =
  let terminal = tcgetattr stdin in
  let echo_state =
    {
      terminal with
      c_echo = true;
      c_icanon = config;
      c_brkint = config;
      c_inpck = config;
      c_istrip = config;
      c_isig = config;
      c_opost = config;
      c_ixon = config;
      c_icrnl = config;
      c_vmin = 0;
      c_vtime = 1;
    }
  in
  tcsetattr stdin TCSAFLUSH echo_state

let clean_lines = 
    let add_clean_line_to_buf = cat (create 0) (of_string "\x1b[2J]") in
    let add_start_line_to_buf = cat add_clean_line_to_buf (of_string "\x1b[H") in
    add_start_line_to_buf

let rec key_loop buf pos =
  let _ = read stdin buf pos 1 in
  match get buf pos with
  (* Handle ctrl-c *)
  | '\003' ->
      let _ = write stdout clean_lines 0 (length clean_lines) in
      ()
  | _ -> key_loop buf (pos + 1)

let rec draw_rows n buf =
  if n = 0 then buf
  else
    let new_buf = cat buf (of_string "~\r\n") in
    draw_rows (n - 1) new_buf

let clean_and_draw_cli_buf = 
  let rows = match Terminal_size.get_rows () with 
  | Some i -> i
  | _ -> 0 in
  let add_draw_row_to_buf = draw_rows rows clean_lines in
  cat add_draw_row_to_buf (of_string "\x1b[H")

let () =
  raw_mode false;
  let _ = write stdout clean_and_draw_cli_buf 0 (length clean_and_draw_cli_buf) in
  let _ = key_loop (create 1024) 0 in
  raw_mode true