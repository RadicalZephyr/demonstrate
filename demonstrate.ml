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

let copy_from_fd fd =
  let read_fds = [fd] in
  let buf = String.create 256 in
  let rec copy_output_itr () =
    let {Unix.Select_fds.read   = read;
         Unix.Select_fds.write  = _   ;
         Unix.Select_fds.except = _   ;} =
      Unix.select ~restart:true
                  ~read:read_fds
                  ~write:[]
                  ~except:[]
                  ~timeout:(`After 0.1) () in
    (* Somehow match against the result to see if anything is ready. *)
    match read with
    | [] -> ()
    | fd :: _ ->
    let read_chars = Unix.read fd ~buf in
    let _ = Unix.write Unix.stdout ~buf ~len:read_chars in
    copy_output_itr ()
  in
  copy_output_itr ()

let send_string_to_interpreter mfd line =
  let _ = Unix.single_write mfd ~buf:line in
  copy_from_fd mfd

let process mfd script_stream =
  let rec prompt_rec () =
    copy_from_fd mfd;
    match (In_channel.input_line stdin) with
    | None -> ()
    | Some line ->
       if String.is_empty line then
         begin
           match input_line_skip_blanks script_stream with
           | None -> prompt_rec ()
           | Some line ->
              send_string_to_interpreter mfd line;
              prompt_rec ()
         end
       else
         begin
           send_string_to_interpreter mfd line;
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
        close master_fd;
        setup_child_fds slave_name;
        let _ = Sid.setsid () in
        let _ = Ioctl.setctty () in
        never_returns (exec ~prog ~args ~use_path:true ())

     | `In_the_parent cpid ->
        (* Do the actual work of feeding lines to the interpreter *)
        In_channel.with_file script ~f:(process master_fd);
        close master_fd;
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
