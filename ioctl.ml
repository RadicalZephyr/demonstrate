open Core.Std
open Ctypes
open Foreign

let tiocsctty = Unsigned.ULong.of_int 536900705

(* int ioctl(int fildes, unsigned long request, ...); *)
let ioctl =
  foreign "ioctl" (int @-> ulong @-> int @-> returning int)

let setctty () =
  ioctl 0 tiocsctty 1
