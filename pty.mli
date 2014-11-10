module Pt_options :
  sig
    type pt_options = ReadWrite | NoCTTY
    val of_int : int -> pt_options
    val to_int : pt_options -> int
  end

val pt_opt : Pt_options.pt_options Ctypes.typ
val file_desc : UnixLabels.file_descr Ctypes.typ

val openpt : Pt_options.pt_options -> UnixLabels.file_descr
val grantpt : UnixLabels.file_descr -> int
val ptsname : UnixLabels.file_descr -> bytes
val unlockpt : UnixLabels.file_descr -> int
