open Core.Std
open Ctypes
open Foreign

let nccs = 20
let tcsanow = 0

let tcflag_t  = ulong
let cc_t      = char
let speed_t   = ulong

type termios
let termios : termios structure typ = structure "termios"
let c_iflag  = field termios "c_iflag"  tcflag_t
let c_oflag  = field termios "c_oflag"  tcflag_t
let c_cflag  = field termios "c_cflag"  tcflag_t
let c_lflag  = field termios "c_lflag"  tcflag_t
let c_cc     = field termios "c_cc"     (array nccs cc_t )
let c_ispeed = field termios "c_ispeed" speed_t
let c_ospeed = field termios "c_ospeed" speed_t
let () = seal termios

let file_desc =
  view int
       ~read:Unix.File_descr.of_int
       ~write:Unix.File_descr.to_int

(* void cfmakeraw(struct termios *termios_p); *)
let cfmakeraw =
  foreign "cfmakeraw" (ptr termios @-> returning void)

(* int tcgetattr(int fildes, struct termios *termios_p); *)
let tcgetattr =
  foreign "tcgetattr" (file_desc @-> ptr termios @-> returning int)

(* int tcsetattr(int fildes, int optional_actions, *)
(*     const struct termios *termios_p); *)
let tcsetattr =
  foreign "tcsetattr" (file_desc @-> int @-> ptr termios @-> returning int)


let make_term_raw fd =
  let orig_settings_ptr = allocate termios (make termios) in
  let _ = tcgetattr fd orig_settings_ptr in
  cfmakeraw orig_settings_ptr;
  let _ = tcsetattr fd tcsanow orig_settings_ptr in
  ()
