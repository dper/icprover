let result = if Array.length Sys.argv = 2 then
    let lexbuf = Lexing.from_string Sys.argv.(1) in
    match Parser.pqs Lexer.lexer lexbuf with
    | question::[] -> 
      (match Proof.search Proof.Classical question with
    | None -> 
			let _ = print_endline "No proof found." in 1
    | Some p -> 
			let _ = print_endline (Proof.to_string p) in 0)
    | _ -> failwith "Internal error: 1 question expected."
  else if Array.length Sys.argv = 1 then
    failwith "Error: Too few arguments.  Needs exactly 1."
  else 
    failwith "Error: Too many arguments.  Needs exactly 1."
in
  exit result
