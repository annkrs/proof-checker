open List
open Core
open Lexer
open Lexing
open Checking
open Common
open Out_channel

(* daily ocaml routine
opam switch 4.05.0 
eval `opam config env`
*)

let printPosition outx lexbuf =
	(* prints position in code where an error occured *)
	let pos = lexbuf.lex_curr_p in
	fprintf outx "%s:%d:%d" pos.pos_fname
		pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
	(* parses lexer output according to given grammar decribed in parser.mly file; prints an error message if read test is incorrect *)
	try Parser.prog Lexer.read lexbuf with
	| SyntaxError msg ->
		fprintf stderr "%a: %s\n" printPosition lexbuf msg; []
	| Parser.Error ->
		fprintf stderr "%a: syntax error\n" printPosition lexbuf; exit (-1)

let rec checkAllProofs lexbuf (env:component list) =
	(* traverses list of proofs and axioms, checks each proof *)
	match lexbuf with
	| [] -> ()
	| h :: t -> 
		match h with
		| ("axiom", goal, []) -> 
			printAxiom (goal); 
			checkAllProofs t (goal :: env)
		| (identifier, goal, proof) -> 
			if (isSome (checkProof env h)) then 
				checkAllProofs t (goal :: env)
			else 
				checkAllProofs t env

let run input () = 
	(* lexer and parser run here *)
	let inx = In_channel.create input in
	let lexbuf = Lexing.from_channel inx in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input };
	checkAllProofs (parse lexbuf) [];
	In_channel.close inx

let () = 
	Core.(
	  Command.basic_spec ~summary:"Natural deduction"
		Command.Spec.(empty +> anon ("filename" %: file))
		run |> Command.run
	)
