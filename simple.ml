open Core.Std

let setup_child_fds slave_name =
  let open Unix in
  let fd = openfile (~mode:[O_RDWR]) slave_name in

  (* Replace all three *)
  dup2 ~src:fd ~dst:stdin;
  dup2 ~src:fd ~dst:stdout;
  dup2 ~src:fd ~dst:stderr

let rec echo_serv () =
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
     Out_channel.output_string stdout (sprintf "Got input: '%s'\n" line);
     Out_channel.flush stdout;
     echo_serv ()

let rec echo_read mfd =
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
     let _ = Unix.single_write mfd ~buf:line in
     let str = String.create 100 in
     let _ = Unix.read mfd ~buf:str in
     printf "%s" str;
     echo_read mfd

let dispatch () =
  (* Setup the pty *)
  let (master_fd, slave_name) = Pty.prepare_pt () in

  let open Unix in
  match fork () with
  | `In_the_child   ->
     close master_fd;
     setup_child_fds slave_name;
     echo_serv ()

  | `In_the_parent _ ->
     echo_read master_fd

let () =
  dispatch ()
