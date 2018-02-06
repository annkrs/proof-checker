{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
	let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- { 
		pos with pos_bol = lexbuf.lex_curr_pos;
			pos_lnum = pos.pos_lnum + 1
	}
}

let identifier = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let var = ['a'-'z']
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
	parse
	| whitespace { read lexbuf }
	| newline  { next_line lexbuf; read lexbuf }
	| "axiom" { AXIOM }
	| "goal" { GOAL }
	| "proof" { PROOF }
	| "end." { END }
	| 'T' { TRUE }
	| 'F' { FALSE }
	| var { VAR (Lexing.lexeme lexbuf) }
	| identifier { IDENT (Lexing.lexeme lexbuf) }
	| '(' { LPAREN }
	| ')' { RPAREN }
	| '[' { LFRAME }
	| ']' { RFRAME }
	| ':' { COLON }
	| ';' { SEMICOLON }
	| '~' { NEG }
	| "/\\" { CON }
	| "\\/" { DIS } 
	| "<=>" { BIC }
	| "=>" { IMP }
	| eof { EOF }
	