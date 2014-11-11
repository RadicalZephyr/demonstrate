open Core.Std


let print_usage () =
  print_string "usage: demonstrate <script> <interpreter> [args...]\n"

let setup_child_fds slave_name =
  let open Unix in
  let fd = openfile (~mode:[O_RDWR]) slave_name in
  Termios.make_term_raw fd;

  (* Replace all three *)
  dup2 ~src:fd ~dst:stdin;
  dup2 ~src:fd ~dst:stdout;
  dup2 ~src:fd ~dst:stderr;
  close fd

let rec input_line_skip_blanks in_ch =
  match In_channel.input_line in_ch with
  | None -> None
  | Some line as result ->
     if String.is_empty line then
       input_line_skip_blanks in_ch
     else
       result

let fprint_from_fd ofd ifd =
  let buff = String.create 256 in
  let rec itr () =
    let read_chars = Unix.read ifd ~buf:buff in
    if read_chars > 0 then
      begin
        let _ = Unix.single_write ofd ~buf:buff ~len:read_chars in
        itr ()
      end
    else
      ()
  in
  itr ()

let process mfd script_stream =
  let rec prompt_rec () =
    fprintf stderr "Input: ";
    Out_channel.flush stderr;
    match (In_channel.input_line stdin) with
    | None -> ()
    | Some line ->
       if String.is_empty line then
         begin
           match input_line_skip_blanks script_stream with
           | None -> ()
           | Some line ->
              let _ = Unix.single_write mfd ~buf:line in
              print_string "Script: ";
              Out_channel.flush stdout;
              fprint_from_fd (Unix.descr_of_out_channel stdout) mfd;
              print_newline ();
              Out_channel.flush stdout;
              prompt_rec ()
         end
       else
         begin
           let _ = Unix.single_write mfd ~buf:line in
           print_string "Output: ";
           Out_channel.flush stdout;
           fprint_from_fd (Unix.descr_of_out_channel stdout) mfd;
           print_newline ();
           Out_channel.flush stdout;
           prompt_rec ()
         end
  in
  prompt_rec ()

let rec echo_serv () =
  let open Unix in
  let str = String.create 100 in
  let read_chars = read stdin ~buf:str in
  let out_line = sprintf "Got input: '%s'" (String.prefix str read_chars) in
  let _ = single_write stdout ~buf:out_line in
  echo_serv ()

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
        close master_fd;
        setup_child_fds slave_name;
        echo_serv ()

     | `In_the_parent cpid ->
        (* Do the actual work of feeding lines to the interpreter *)
        In_channel.with_file script ~f:(process master_fd);
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
