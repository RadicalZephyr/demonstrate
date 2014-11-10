open Core.Std


let print_usage () =
  print_string "usage: demonstrate <script> <interpreter> [args...]\n"


let demonstrate script command =
  (* Setup the pty *)

  (* Then fork and exec the interpreter *)
  match command with
  | [] -> assert false (* Should not be able to get here. *)
  | prog :: _ as args ->
     let pid = Unix.fork_exec ~prog ~args ~use_path:true () in
     try
       let _ = Unix.waitpid pid in
       ()
     with
     | Unix.Unix_error (err, _, _) ->
        Out_channel.output_string stderr (Unix.error_message err)



let () =
  match (Array.to_list Sys.argv) with
  (* If we don't get the right number of arguments, print out the usage
  message. *)
  | []
  | _ :: []
  | _ :: _ :: [] ->
     print_usage ()

  | _ :: script :: command ->
     demonstrate script command
