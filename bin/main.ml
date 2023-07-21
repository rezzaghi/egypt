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

let rec main_loop buf pos =
  let _ = read stdin buf pos 1 in
  match get buf pos with
  (* Handle ctrl-c *)
  | '\003' ->
      let _ = write stdout (of_string "\x1b[2J") 0 4 in
      let _ = write stdout (of_string "\x1b[H") 0 3 in
      ()
  | _ -> main_loop buf (pos + 1)

let rec draw_rows n =
  if n = 0 then ()
  else
    let _ = write stdout (of_string "~\r\n") 0 3 in
    draw_rows (n - 1)

let () =
  raw_mode false;
  (* before cli buffer*)
  let _ = write stdout (of_string "\x1b[2J") 0 4 in
  let _ = write stdout (of_string "\x1b[H") 0 3 in
  let rows = match Terminal_size.get_rows () with 
  | Some i -> i
  | _ -> 0 in
  draw_rows rows;
  let _ = write stdout (of_string "\x1b[H") 0 3 in
  let _ = main_loop (create 1024) 0 in
  raw_mode true
