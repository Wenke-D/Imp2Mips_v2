open Format

let () = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let prog =
    try Impparser.program Implexer.token lb
    with Failure msg ->
      print_endline msg;
      exit 1
  in
  close_in c;
  let asm = Imp2mips.translate_program prog in
  let output_file = Filename.chop_suffix file ".imp" ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
