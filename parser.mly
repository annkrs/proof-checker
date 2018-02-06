%{ open Common %}

%token AXIOM
%token GOAL
%token PROOF
%token END

%token <string> IDENT
%token <string> VAR

%token LPAREN
%token RPAREN
%token LFRAME 
%token RFRAME

%token COLON
%token SEMICOLON

%token TRUE 
%token FALSE
%token NEG
%token CON 
%token DIS
%token IMP 
%token BIC

%left BIC
%right IMP
%left DIS
%left CON

%token EOF

%start < (string * Common.component * Common.component list) list > prog
%%

prog:
	| AXIOM; COLON; goal = nGoal; END; remainder = prog
		{ ("axiom", goal, []) :: remainder }
	| GOAL; identifier = IDENT; COLON; goal = nGoal;
		PROOF; proof = nProof; END; remainder = prog
		{ (identifier, goal, proof) :: remainder }
	| EOF 
		{ [] }
;

nGoal:
	| LPAREN; elem = nGoal; RPAREN 
		{ elem }
	| operand = nUop 
		{ operand }
	| operand = nBiop 
		{ operand }
	| operand = nAtom 
		{ operand }
;

nUop:
	| NEG; LPAREN; elem = nGoal; RPAREN 
		{ (Neg elem) }
	| NEG; operand = nUop 
		{ Neg operand }
	| NEG; operand = nAtom 
		{ Neg operand }
;

nBiop:
	| leftOp = nGoal; CON; rightOp = nGoal 
		{ (Con (leftOp, rightOp)) }
	| leftOp = nGoal; DIS; rightOp = nGoal 
		{ (Dis (leftOp, rightOp)) }
	| leftOp = nGoal; BIC; rightOp = nGoal 
		{ (Bic (leftOp, rightOp)) }
	| leftOp = nGoal; IMP; rightOp = nGoal 
		{ (Imp (leftOp, rightOp)) }
;

nAtom:
	| elem = VAR 
		{ (Var elem) }
	| TRUE 
		{ True }
	| FALSE 
		{ False }
;

nProof:
	| LFRAME; assumpion = nGoal; COLON; remOfFrame = nFrameComponents; RFRAME; SEMICOLON; remainder = nProof 
		{ Frame (assumpion, remOfFrame) :: remainder }
	| component = nGoal; SEMICOLON; remainder = nProof 
		{ component :: remainder }
	| component = nGoal; 
		{[component]}
	| { [] }
;

nFrameComponents:
	| LFRAME; assumpion = nGoal; COLON; remOfFrame = nFrameComponents; RFRAME; SEMICOLON; remainder = nFrameComponents 
		{ Frame (assumpion, remOfFrame) :: remainder }
	| component = nGoal; SEMICOLON; remainder = nFrameComponents 
		{ component :: remainder }
	| component = nGoal; 
		{ [component] }
;
