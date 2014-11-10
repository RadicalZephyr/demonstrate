open Core.Std


let print_usage () =
  print_string "usage: demonstrate <script> <interpreter> [args...]\n"

let print_args script interpreter args =
  printf "Process script: '%s' with interpreter: '%s'\n with args '%s'\n"
         script interpreter (String.concat args ~sep:", ")

let () =
  match (Array.to_list Sys.argv) with
  | [] | _ :: [] | _ :: _ :: [] -> print_usage ()
  | _ :: script :: interpreter :: args ->
       print_args script interpreter args
