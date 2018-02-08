open List
open Common


(* TODO
-change result type to option 
-elimination rule for 'F' -> everything is achievable *)


(* AUXILIARY FUNCTIONS *)

let isFrame (comp:component) = 
	(* checks if component is a Frame *)
	match comp with
		| Frame (_, _) -> true
		| _ -> false 

let allFrames (env:component list) =
	(* returns list of all frames from environment *) 
	List.filter isFrame env

let framesEndingWithComponent (comp:component) (env:component list) = 
	(* returns a list of all Frame from environment ending with given component *)
	List.filter (fun x -> 
		match x with
		| Frame (a, fp) -> (List.hd (List.rev fp)) = comp
		| _ -> failwith "tried to find last element of frame proof; frame expected") 
		(allFrames env)

let existsFrame (assum:component) (last:component) (env:component list) =
	(* checks if exists a frame with assumption = assum ending with component = last in environment env *)
	List.exists (fun x -> 
		match x with
		| Frame (a, fp) -> a = assum
		| _ -> failwith "tried to search frames; frame expected") 
		(framesEndingWithComponent last env)

let isImp (comp:component) = 
	(* checks if component is an Imp *)
	match comp with
		| Imp (_, _) -> true
		| _ -> false 

let allImps (env:component list) =
	(* returns list of all Imp from environment *) 
	List.filter isImp env

(* DEDUCING *)

let rec isDerivable (axioms:component list) (expr:component) (env:component list) = 
	(* tries to derive by checking if exists similar axiom or expression is achievable using inference rules *)

	let existsSimilarAxiom (axioms:component list) (expr:component) = 
		(* checks if in axioms (list of axioms read so far from input file) exists expression similar ("isomorphic") to given expr*)
		false in

	let isAchievable (expr:component) (env:component list) = 
		(* checks whether is it possible to deduce expression from environment using inference rules *)
		
		let wasIntroduced = 
			(* checks possibility to introduce connective from environment *)
			match expr with 
			| True -> true 
			| Neg (Neg (x)) -> 
				isDerivable axioms x env
			| Neg (x) -> 
				existsFrame x False env || (* modus tollens below *)
					List.exists (fun x -> 
						match x with 
						| Imp (a, b) -> List.mem (Neg (b)) env
						| _ -> failwith "tried to check modus tollens; received component is not an Imp") 
					(allImps env)
			| Con (x, y) -> 
				isDerivable axioms x env && isDerivable axioms y env
			| Dis (x, y) -> 
				isDerivable axioms x env || isDerivable axioms y env
			| Bic (x, y) -> 
				isDerivable axioms (Imp (x, y)) env && isDerivable axioms (Imp (y, x)) env
			| Imp (x, y) -> 
				existsFrame x y env 
			| _ -> failwith "tried to check if component was introduced; received component is a frame" in 

		let rec isResultOfElimination (lst:component list) = 
			(* checks if expression is obtained from connective elimination by traversing environment and trying to apply elimination rule to each of elements *)

			let eliminatesConnective (comp:component) = 
				(* tries to apply elimination rules to elements of environment *)
				match comp with 
				| Con (x, y) -> x = expr || y = expr
			 	| Imp (x, y) -> List.mem x env && y = expr
			 	| _ -> failwith "not implemented" in

			match env with 
			| h :: t -> 
				if eliminatesConnective h
					then true
				else isResultOfElimination t 
			| [] -> false in

		wasIntroduced || isResultOfElimination env in (* end of isAchievable *)

	(List.mem expr env) || (existsSimilarAxiom axioms expr) || (isAchievable expr env)


(* CHECKING PROOF *)

let checkProof (axioms:component list) ((identifier, goal, proof):string * component * component list) =
	(* checks whether proof is correct and prints answer *)

	let rec check (proof:component list) (env:component list) =
		(* traverses proof - list of components *)
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

	match (check proof [True]) with 
	| (res, env) ->  
		if res &&  List.mem goal env	
			then (print_string (identifier ^ " is correct\n"); printProof (goal); true)
		else
			(print_string (identifier ^ " is not correct\n"); printProof (goal); false)
