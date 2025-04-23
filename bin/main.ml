open Printf

let read_file filename =
  try
    let ic = open_in filename in  (* Open the file for reading *)
    let n = in_channel_length ic in (*get the length of the input channel*)
    let s = really_input_string ic n in (*read all the content*)
    close_in ic;                  (* Close the file *)
    s
  with
  | Sys_error msg ->
      eprintf "Error: %s\n" msg;  (* Handle file-related errors *)
      "" (* Return empty string in case of error *)

let main filename =
  let content = read_file filename in
  printf "File content:\n%s\n" content

let () =
  if Array.length Sys.argv <> 2 then
    eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    main Sys.argv.(1)
