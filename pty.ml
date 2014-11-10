open Core.Std
open Ctypes
open Foreign

module Pt_options = struct
    type pt_options =
      | ReadWrite
      | NoCTTY

    let of_int num =
      match num with
      | 0 -> ReadWrite
      | 1 -> NoCTTY
      | x -> failwith (sprintf "Error, cannot use %d as an option for openpt" x)

    let to_int opt =
      match opt with
      | ReadWrite -> 0
      | NoCTTY    -> 1
  end

let pt_opt =
  view int
       ~read:Pt_options.of_int
       ~write:Pt_options.to_int

let file_desc =
  view int
       ~read:Unix.File_descr.of_int
       ~write:Unix.File_descr.to_int

let openpt =
  foreign "posix_openpt" (pt_opt @-> returning file_desc)

let grantpt =
  foreign "grantpt" (file_desc @-> returning int)

let ptsname =
  foreign "ptsname" (file_desc @-> returning string)

let unlockpt =
  foreign "unlockpt" (file_desc @-> returning int)
