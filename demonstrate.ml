open Core.Std


let print_usage () =
  print_string "usage: demonstrate <script> <interpreter> [args...]\n"

let setup_child_fds slave_name =
  let open Unix in
  let fd = openfile (~mode:[O_RDONLY]) slave_name in
  dup2 ~src:fd ~dst:stdin

let process master_ostream script_stream =
    let rec prompt_rec () =
    match (In_channel.input_line stdin) with
    | None -> ()
    | Some line ->
       if (String.length line) = 0 then
         begin
           match (In_channel.input_line script_stream) with
           | None -> prompt_rec ()
           | Some line ->
              Out_channel.output_string master_ostream line;
              prompt_rec ()
         end
       else
         begin
           Out_channel.output_string master_ostream line;
           prompt_rec ()
         end
  in
  prompt_rec ()

let demonstrate script command =
  (* Setup the pty *)
  let (master_fd, slave_name) = Pty.prepare_pt () in

  (* Then fork and exec the interpreter *)
  match command with
  | [] -> assert false (* Should not be able to get here. *)
  | prog :: _ as args ->
     let open Unix in
     match fork () with
     | `In_the_child   ->
        (* Setup the input/output file descriptors *)
        setup_child_fds slave_name;

        never_returns (exec ~prog ~args ~use_path:true ())

     | `In_the_parent cpid ->
        (* Do the actual work of feeding lines to the interpreter *)
        let master_ostream = Unix.out_channel_of_descr master_fd in
        In_channel.with_file script ~f:(process master_ostream);

        try
          let _ = waitpid cpid in
          ()
        with
        | Unix_error (err, _, _) ->
           Out_channel.output_string Out_channel.stderr (error_message err)

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
