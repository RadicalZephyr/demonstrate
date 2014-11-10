open Ctypes
open Foreign

let openpt =
  foreign "posix_openpt" (int @-> returning int)

let grantpt =
  foreign "grantpt" (int @-> returning string)

let ptsname =
  foreign "ptsname" (int @-> returning int)

let unlockpt =
  foreign "unlockpt" (int @-> returning int)
