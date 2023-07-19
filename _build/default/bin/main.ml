open Unix
open Bytes

let raw_mode echo =
  let terminal = tcgetattr stdin in
  let echo_state =
    if echo = true then { terminal with c_echo = true; c_icanon = false; c_isig = false }
    else { terminal with c_echo = true; c_icanon = true }
  in
  tcsetattr stdin TCSAFLUSH echo_state

let rec main_loop pos buf =
  let _ = raw_mode true in
  let _ = read stdin buf pos 1 in
  if get buf pos = Char.chr 4 then
    print_bytes buf
  else main_loop (pos + 1) buf

let () = main_loop 0 (create 1024)
