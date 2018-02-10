open List
open Common

(* TODO: 
-or elimination
-axioms *)


(* AUXILIARY FUNCTIONS *)

let allNegs (env:component list) = 
	(* returns list of all negations from environment *)

	let isNeg (comp:component) = 
		(* checks if component is a negation *) 
		match comp with 
		| Neg (_) -> true 
		| _ -> false in

	List.filter isNeg env

let allFrames (env:component list) =
	(* returns list of all frames from environment *) 

	let isFrame (comp:component) = 
		(* checks if component is a frame *)
		match comp with
			| Frame (_, _) -> true
			| _ -> false in

	List.filter isFrame env

let framesEndingWithComponent (comp:component) (env:component list) = 
	(* returns a list of all frames from environment ending with given component *)
	List.filter (fun x -> 
		match x with
		| Frame (a, fp) -> (List.hd (List.rev fp)) = comp
		| _ -> failwith "tried to find last element of frame proof; received component is not a frame") 
		(allFrames env)

let existsFrame (assum:component) (last:component) (env:component list) =
	(* checks if exists a frame with assumption = assum ending with component = last in environment env *)
	List.exists (fun x -> 
		match x with
		| Frame (a, fp) -> a = assum
		| _ -> failwith "tried to search frames; received component is not a frame") 
		(framesEndingWithComponent last env)

let allImps (env:component list) =
	(* returns list of all implications from environment *) 

	let isImp (comp:component) = 
		(* checks if component is an implication *)
		match comp with
			| Imp (_, _) -> true
			| _ -> false in
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
			| False -> 
				List.exists (fun v -> 
					match v with 
					| Neg (x) -> List.mem x env
					| _ -> failwith "tried to introduce negation; negation component expected") 
				(allNegs env)
			| Var (x) -> false
			| Neg (Neg (x)) -> 
				isDerivable axioms x env
			| Neg (x) -> 
				existsFrame x False env || (* modus tollens below *)
					List.exists (fun v -> 
						match v with 
						| Imp (x, y) -> List.mem (Neg (y)) env
						| _ -> failwith "tried to check modus tollens; received component is not an Imp") 
					(allImps env)
			| Con (x, y) -> 
				isDerivable axioms x env && isDerivable axioms y env
			| Dis (x, y) -> 
				(List.mem (Dis (y, x)) env) ||
				isDerivable axioms x env || isDerivable axioms y env
			| Bic (x, y) -> 
				isDerivable axioms (Imp (x, y)) env && isDerivable axioms (Imp (y, x)) env
			| Imp (x, y) -> 
				existsFrame x y env 
			| _ -> false in 

		let rec isResultOfElimination (lst:component list) = 
			(* checks if expression is obtained from connective elimination by traversing environment and trying to apply elimination rule to each of elements *)

			let eliminatesConnective (comp:component) = 
				(* tries to apply elimination rules to elements of environment *)
				(print_string (debugMessage env comp expr)); 
				match comp with 
				| False -> true
				| Neg (Neg (x)) -> 
					expr = x
				| Neg (x) -> 
					expr = False && List.mem x env
				| Con (x, y) -> 
					expr = x || expr = y
				| Bic (x, y) -> 
					(expr = x && List.mem y env) 						||
					(expr = y && List.mem x env) 						||
					(expr = Neg (x) && List.mem (Neg (y)) env) 			||
					(expr = Neg (y) && List.mem (Neg (x)) env) 			||
					(expr = Con (x, y) && List.mem (Dis (x, y)) env) 	||
					(expr = Con (Neg (x), Neg (y)) && 
						List.mem (Dis (Neg (x), Neg (y))) env)
				| Imp (x, y) -> 
					y = expr && List.mem x env 
				| _ -> false in

			match lst with 
			| h :: t -> 
				if eliminatesConnective h then 
					true
				else 
					isResultOfElimination t 
			| [] -> false in

		wasIntroduced || isResultOfElimination env in (* end of isAchievable *)

	(List.mem expr env) || (existsSimilarAxiom axioms expr) || (isAchievable expr env)


(* CHECKING PROOF *)

let checkProof (axioms:component list) ((identifier, goal, proof):string * component * component list) =
	(* checks whether proof is correct and prints answer *)

	let rec check (proof:component list) (env:component list) =
		(* traverses proof - list of components *)
		match proof with 
		| [] -> Some (env)
		| (Frame (assum, frameProof)) :: components ->  
			if (isSome (check frameProof (assum :: env))) then 
				check components (Frame (assum, frameProof) :: env)
			else 
				None
		| (h :: t) -> 
			if isDerivable axioms h env then 
				check t (h :: env)
			else 
				None in

	(* print proof identifier + good/bad news; print goal; return result *)
	let result = check proof [True] in 
	((if (isSome result) && List.mem goal (get result) then
		(print_string (identifier ^ " is correct\n"))
	else
		(print_string (identifier ^ " is not correct\n"))); 
	
	printProof goal); 
	result 
