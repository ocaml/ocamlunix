(*----------------------------------------------------------------------------
  Copyright (c) 2009, Daniel C. Buenzli
 
  All rights reserved. Distributed under a creative commons
  attribution-non-commercial-share alike 2.0 France license.
  http://creativecommons.org/licenses/by-nc-sa/2.0/fr/
 ----------------------------------------------------------------------------*)

open Ocamlbuild_plugin;;

(* pdfLatex, Makeindex, Pgf/TikZ picture extraction to PNG, HeVeA and
   HaChA generic rules. [master_tex] is the pathname to the master tex file. *)

let latex_rules master_tex = 
  let master_html = Pathname.update_extension "html" master_tex in
  let pdflatex = "pdflatex" in
  let pdflatex_cmd dir tags specs = 
    Cmd (S ([A pdflatex; A "-file-line-error"; A "-halt-on-error"; 
	     A "-interaction=nonstopmode"; 
	     A ("-output-directory=" ^ dir);
	     T (tags ++ "compile" ++ "LaTeX")] @ specs))
  in
  let makeindex = "makeindex" in
  let ghostscript = "gs" in
  let hevea = "hevea" in
  let hacha = "hacha" in

  rule "pdfLaTeX for %.tex to %.idx, %.code" 
    ~prods:["%.idx"; "%.code"] 
    ~dep:"%.tex" 
    begin fun env build -> 
      let tex = env "%.tex" in
      pdflatex_cmd (Pathname.dirname tex) (tags_of_pathname tex) [P tex]
    end;

  rule "MakeIndex for %.idx to %.ind-> " 
    ~prod:"%.ind" 
    ~dep:"%.idx"
    begin fun env build ->
      let idx = env "%.idx" in
      Cmd (S [A makeindex;
	      T (tags_of_pathname idx ++ "compile" ++ "MakeIndex");
	      P idx])
    end;

  rule "LaTeX to PDF with index" 
    ~prod:"%.pdf"
    ~dep:"%.ind"
    begin fun env build ->
      let tex = env "%.tex" in
      let cmd = pdflatex_cmd (Pathname.dirname tex) (tags_of_pathname tex)
	  [P tex] 
      in 
      Seq [cmd; cmd ]
    end;

  rule "Extract LaTeX PGF/TikZ pictures as PDF"
    ~prod:"%-image.pdf"
    ~dep:"%.tex"
    begin fun env build ->
      let tex = env "%.tex" in
      let tex_tags = tags_of_pathname tex in
      let base = env "%-image" in
      pdflatex_cmd (Pathname.dirname tex) tex_tags 
	[A ("-jobname=" ^ (Pathname.basename base));
	 A ("\\PassOptionsToPackage{active}{preview}\\input{" ^ tex ^ "}")]
    end;

  rule "PDF to PNGs" 
    ~stamp:"%.pngs"
    ~dep:"%.pdf"
    begin fun env build ->
      let pdf = env "%.pdf" in
      let pdf_tags = tags_of_pathname pdf in 
      let base = env "%" in
      Cmd (S [A ghostscript;
	      A "-dNOPAUSE"; A "-dBATCH";
	      A "-dGraphicsAlphaBits=4"; A "-dTextAlphaBits=4";
	      A "-r600";
	      A "-sDEVICE=png16m";
	      A ("-sOutputFile=" ^ base ^ "%d.png");
	      T (pdf_tags ++ "compile" ++ "Ghoscript" ++ "png");
	      P pdf])
    end;
  
  rule "LaTeX to monolithic html" 
    ~prod:"%.html" 
    ~deps:["%.tex"; "%-image.pngs"]
    begin fun env build ->
      let tex = env "%.tex" in
      let html = env "%.html" in
      let tags = tags_of_pathname tex ++ "compile" ++ "LaTeX" ++ "html" in
      Cmd (S [A hevea; 
	      A "book.hva";
	      S [A "-I"; A (Pathname.dirname tex)];
	      S [A "-o"; Px html];
	      A "-fix"; A "-O"; S [A "-exec"; A "xxdate.exe"];
	      T tags;
	      P tex;])
    end;

  rule "HaChA deconstruction" 
    ~prod:"index.html"
    ~dep:master_html
    begin fun env build -> 
      let out = (Pathname.dirname master_html) ^ "/index.html" in
      Cmd (S [A hacha; S [A "-o"; A out]; P master_html])
    end
;;

(* Rules specific to the ocamlunix project *)

let pathroot s = "src/" ^ s
let master_tex = pathroot ("ocamlunix.tex")
let ocamlversion = "ocamlversion.tex"
let deps = List.map pathroot [
  ocamlversion;
  "prelude.sty"; "prelude.hva"; 
  "front.tex"; "intro.tex"; "general.tex"; "files.tex"; "processes.tex";
  "signals.tex"; "pipes.tex"; "sockets.tex"; "threads.tex"; "more.tex"; 
  "references.tex"; "data/speed-log.data"]

(* Outputs Sys.ocaml_version to ocamlversion.tex *)
let ocamlversion_rule () = 
  let echo _ _ = Echo ([Sys.ocaml_version], (pathroot ocamlversion)) in
  rule "ocamlversion.tex generation" ~prod:(pathroot ocamlversion) echo
;;

(* Converts (x,y) .data file to (log10 x, log10 y) .data file *)
let datalog10_rule () =
  let log10 env _ =
    let nums = List.map float_of_string (string_list_of_file (env "%.data")) in
    let x_y = 
      let pair (fst, acc) v = match fst with 
      | None -> (Some v, acc) 
      | Some fst -> (None, (fst, v) :: acc)
      in
      snd (List.fold_left pair (None, []) nums)
    in
    let logx_logy = 
      let ll (x, y) = Printf.sprintf " %.4f %.4f\n" (log10 x) (log10 y) in
      List.rev_map ll x_y
    in
    Echo (logx_logy, env "%-log.data")
  in
  rule "log-log from %.data generation" ~prod:"%-log.data" ~dep:"%.data" log10
;;


(* extracts file from %.code file to the code directory *)  
let file_extract_rule () =
  let lines_of_file f =                                     (* with endlines *)
    let clean_line l =     (* drop $.*$, except \indexlibvalue and \libvalue *) 
      try 
	let len = String.length l in 
	let s = String.index l '$' in
	let e = String.rindex l '$' in     (* assumes a single $.*$ per line *)
	try
	  if s + 7 > len then raise Exit else
	  match String.sub l (s + 1) 7 with 
	  | "\\libval" | "\\indexl" | "\\libexn" | "\\libtyp"->
	      let s1 = String.index_from l s '{' in           (* module name *)
	      let e1 = String.index_from l s '}' in  
	      let s2 = String.index_from l (s1 + 1) '{' in     (* value name *)
	      let e2 = String.index_from l (e1 + 1) '}' in
              let rem_bslash id =
		let acc = ref (String.copy "") in
		let add_c c = if c <> '\\' then acc := !acc ^ String.make 1 c
		in
		String.iter add_c id;
		!acc
	      in
	      (String.sub l 0 s) ^
	      (rem_bslash (String.sub l (s2 + 1) (e2 - s2 - 1))) ^
	      (String.sub l (e + 1) (len - e - 1))
	  | _ -> raise Exit
	with Exit -> (String.sub l 0 s) ^ (String.sub l (e + 1) (len - e - 1))
      with Not_found -> l
    in
    let i = open_in f in 
    let rec aux i acc = 
      match try Some (clean_line (input_line i)) with End_of_file -> None with
      | None -> List.rev acc
      | Some l -> aux i ((l ^ "\n") :: acc)
    in
    aux i []
  in
  let snippets_of_lines base lines =                             (* reversed *)
    let snip_header = function 
      | file :: source :: line :: tail -> 
	  let file = (String.sub file 1 (String.length file - 2)) in
	  let source = String.sub source 0 (String.length source -1) in
	  let line = String.sub line 0 (String.length line - 1) in
	  (file, [ Printf.sprintf "#%s \"%s%s\"\n" line base source ]), tail
      | _ -> failwith "wrong file format"
    in
    let rec aux (f, c as current) acc = function
      | [] -> current :: acc
      | (l :: _) as list when (String.length l <> 0 && l.[0] = '=') ->
	  let snip, list' = snip_header list in
	  aux snip (current :: acc) list'
      | (l :: tail) -> aux (f, l :: c) acc tail
    in
    let snip, list' = snip_header lines in 
    aux snip [] list'
  in
  let merge_snippets snips =
    let rec aux (f, c as current) acc = function
      | [] -> List.rev (current :: acc)
      | (f', c') :: tail when f = f' -> aux (f, List.rev_append c' c) acc tail
      | (f', c') :: tail -> aux (f', List.rev c') (current :: acc) tail
    in
    List.tl (aux ("", []) [] snips)
  in
  let extract env _ =
    let base = Pathname.dirname (env "%") ^ "/" in
    let outdir = base ^ "../../code/" in
    let cmp s s' = Pervasives.compare (fst s) (fst s') in
    let snips = snippets_of_lines base (lines_of_file (env "%.code")) in
    let files = merge_snippets (List.stable_sort cmp snips) in
    Seq (List.map (fun (f, c) -> Echo(c, outdir ^ f)) files)
  in
  rule "code extraction from %.code to code directory" 
    ~stamp:"%.extract"
    ~dep:"%.code" extract;    
;;


dispatch begin function 
  | After_rules -> 
      latex_rules master_tex;
      ocamlversion_rule ();
      datalog10_rule ();
      file_extract_rule ();
      dep ["file:" ^ master_tex] deps
  |  _ -> ()
end
