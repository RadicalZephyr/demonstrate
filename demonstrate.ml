open Core.Std


let print_usage () =
  print_string "usage: demonstrate <script> <interpreter> [args...]\n"


let demonstrate script interpreter args =
  let rec demo_rec () =
    match (In_channel.input_line stdin) with
    | None -> ()
    | Some line ->
       demo_rec ()
  in
  demo_rec ()


let () =
  match (Array.to_list Sys.argv) with
  (* If we don't get the right number of arguments, print out the
  usage message. *)
  | []
  | _ :: []
  | _ :: _ :: [] ->
     print_usage ()

  | _ :: script :: interpreter :: args ->
       demonstrate script interpreter args
