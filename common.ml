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

let isSome (elem:component list option) = 
	(* checks whether elem has a value *)
	match elem with
	| Some _ -> true 
	| None -> false

let get (elem:component list option) = 
	(* obtains the value inside Some *)
	match elem with
	| Some x -> x
	| None -> failwith "treid to obtain value inside Some; received item is a None"

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
	| Frame (x, y) ->
		"[" ^ (componentToString x) ^ ": " ^ String.concat ", " (List.map (fun v -> componentToString v) y) ^ "] "
	(*| _ -> failwith "tried to create string representation of a component; non-framed component expected"*)

let printAxiom (goal:component) = 
	(* prints Axiom with its goal *)
	print_string ("Axiom\n" ^ (componentToString goal) ^ "\n\n")

let printProof (goal:component) =
	(* prints proof's goal *)
	print_string ("goal: " ^ (componentToString goal) ^ "\n\n")

let debugMessage (env:component list) (comp:component) (expr:component) = 
	(* creates string representation of actual state of env *)
	let envToString = 
		String.concat ", " (List.rev (List.map (fun x -> componentToString x) env)) in
	"env: " ^ envToString ^ "\ncomp: " ^ (componentToString comp) ^ 
	" \nexpr: " ^ (componentToString expr) ^ "\n\n"
	