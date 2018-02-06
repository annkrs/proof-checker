type component =
	| True
	| False
	| Var of string
	| Neg of component
	| Con of component * component
	| Dis of component * component
	| Bic of component * component
	| Imp of component * component
	| Frame of component * (component list)

let rec componentToString (comp:component) = 
	(* creates string representation of component *)
	match comp with 
	| True -> "T"
	| False -> "F"
	| Var x -> x
	| Neg x -> 
		"~" ^ (componentToString x)
	| Con (x, y) -> 
		"(" ^ (componentToString x) ^ " /\\ " ^ (componentToString y) ^ ")"
	| Dis (x, y) -> 
		"(" ^ (componentToString x) ^ " \\/ " ^ (componentToString y) ^ ")"
	| Bic (x, y) ->
		"(" ^ (componentToString x) ^ " <=> " ^ (componentToString y) ^ ")"
	| Imp (x, y) -> 
		"(" ^ (componentToString x) ^ " => " ^ (componentToString y) ^ ")"
	| _ -> failwith "tried to create string representation of a component; non-framed component expected"

let printAxiom (goal:component) = 
	(* prints Axiom with its goal *)
	print_string ("Axiom\n" ^ (componentToString goal) ^ "\n\n")

let printProof (goal:component) =
	(* prints proof's goal *)
	print_string ("goal: " ^ (componentToString goal) ^ "\n\n")
	