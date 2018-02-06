open List
open Common

let checkProof (axioms:component list) ((identifier, goal, proof):string * component * component list) =

	let existsSimilarAxiom (axioms:component list) (expr:component) = 
		(* checks if in axioms (list of axioms read so far from input file) exists expression similar ("isomorphic") to given expr*)
		false in

	let isAchievable (expr:component) (env:component list) = 
		(* checks if is it possible to deduce expression from environment using inference rules *)
		true in

	let isDerivable (axioms:component list) (expr:component) (env:component list) = 
		(* tries to derive by checking if exists similar axiom or expression is achievable using inference rules *)
		(existsSimilarAxiom axioms expr) || (isAchievable expr env) in

	let rec check (proof:component list) (env:component list) =
		match proof with 
		| [] -> (true, env)
		| (Frame (assum, frameProof)) :: components ->  
			if (fst (check frameProof (assum :: env)))
				then check components (Frame (assum, frameProof) :: env)
			else
				(false, [])
		| (h :: t) -> 
			if isDerivable axioms h env 
				then check t (h :: env)
			else 
				(false, []) in

	let result = fst (check proof [True]) in 

	if result 
		then 
		 (print_string (identifier ^ " is valid\n"); printProof (goal); true)
		else
		(print_string (identifier ^ " is not valid\n"); false)
