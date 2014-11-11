open Core.Std
open Ctypes
open PosixTypes
open Foreign

       (* pid_t setsid(void); *)
let setsid =
  foreign "setsid" (void @-> returning pid_t)
