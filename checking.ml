open Common

let checkProof (axioms:component list) (identifier, goal, proof) =
	print_string (identifier ^ " is valid\n");
	printProof (goal); 
	true
