let fatal_error fmt =
  Format.ksprintf 
    (fun s -> 
      Printf.printf "Fatal Error: %s\n%!" s;
      exit 1
    ) fmt
let rec mapfind f = function 
  | [] -> []
  | hd :: tl -> 
    match f hd with
    | None -> mapfind f tl
    | Some x -> x :: mapfind f tl

let system_root = 
  match Sys.getenv_opt "SYSTEMROOT" with 
  | None -> fatal_error "Cannot read %%SYSTEMROOT%%" 
  | Some path -> path

let roots = [
  "PROGRAMFILES";
  "PROGRAMFILES(X86)"
]

let rec iter_files max_depth root f = 
  let files = Sys.readdir root in 
  Array.iter (fun file ->
    let path = Filename.concat root file in 
    f path; 
    if max_depth > 0 && Sys.is_directory path then 
      iter_files (max_depth - 1) path f
  ) files

let find_files max_depth root f =
  let result = ref [] in  
  iter_files max_depth root (fun path ->
      if f path then 
        result := path :: !result 
    ); 
  !result

let string_starts ?(from = 0) s p =
  let exception Stop in 
  try 
    String.iteri 
      (fun j c ->
        if s.[from + j] <> c then 
          raise Stop   
      ) p;
    true
  with Stop -> false

let string_contains s p = 
  let exception Found in
  try 
    String.iteri 
      (fun from _ -> 
        if string_starts ~from s p then 
          raise Found
      ) s;
    false 
  with Found -> true

let find_visual_studios root =
  let may_be_visual_studio path =
    string_contains (Filename.basename path) "Visual Studio"   
  in
  let visual_studios = find_files 0 root may_be_visual_studio in 
  let is_batch_file path =
     match Filename.basename path with 
     | "vcvars32.bat" -> false
     | "vcvars64.bat" -> false
     | "VsDevCmd.bat" -> true
     | _ -> false
  in
  let files = 
    mapfind 
      (fun root -> 
        let files = find_files max_int root is_batch_file in 
        if files = [] then 
          None
        else 
          Some (Filename.basename root, files) 
      )
      visual_studios  
  in
  files


let program_files =
  mapfind Sys.getenv_opt roots

let visual_studios = 
  List.flatten (List.map find_visual_studios program_files)

let cmd = Filename.concat system_root "system32/cmd.exe"

let read_lines ic = 
  let rec loop acc = 
    try
      let line = Pervasives.input_line ic in 
      loop (line :: acc)
    with End_of_file -> List.rev acc
  in 
  loop []

let _environment_variables batch = 
  let args =
    Printf.sprintf "%s & set" (Filename.quote batch)
  in 
  let output = Unix.open_process_in (Printf.sprintf "%s /c %s" (Filename.quote cmd) (Filename.quote args)) in 
  let lines = read_lines output in
  List.iter 
    (fun line ->
      match String.split_on_char '=' line with 
      | [name; value] ->
        Printf.printf "%s = %s\n%!" name value;  
        Unix.putenv name value
      | _ -> ()
    ) lines


let powershell batch = 
  let args =
    Printf.sprintf "\"%s\" -arch=amd64 -host_arch=amd64 & start powershell" batch
  in
  Printf.printf "%s\n%!" args;
  Unix.execv cmd [|"/c"; args|] 
 
 let () = 
  match visual_studios with 
  | (_, batch :: _) :: _ -> powershell batch
  | _ -> print_endline "Not found"