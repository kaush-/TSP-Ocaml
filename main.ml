(* An implementation of TSP Solution using Genetic Algorithm********************************************************************************
	Author: Piyush Kaushik *****************************************************************************************************************
	Date: 20 July 2014**********************************************************************************************************************
*)

let main =
	let sample = [['A';'B';'E';'C';'D'];['D';'E';'A';'C';'B'];['A';'C';'D';'B';'E'];['C';'E';'A';'D';'B']] in
	print_string (""^soc (List.nth (List.nth sample 0) 0)^soc (List.nth (List.nth sample 0) 1)^soc (List.nth (List.nth sample 0) 2)^soc (List.nth (List.nth sample 0) 3)^soc (List.nth (List.nth sample 0) 4)^"\n"^soc (List.nth (List.nth sample 1) 0)^soc (List.nth (List.nth sample 1) 1)^soc (List.nth (List.nth sample 1) 2)^soc (List.nth (List.nth sample 1) 3)^soc (List.nth (List.nth sample 1) 4)^"\n"^soc (List.nth (List.nth sample 2) 0)^soc (List.nth (List.nth sample 2) 1)^soc (List.nth (List.nth sample 2) 2)^soc (List.nth (List.nth sample 2) 3)^soc (List.nth (List.nth sample 2) 4)^"\n"^soc (List.nth (List.nth sample 3) 0)^soc (List.nth (List.nth sample 3) 1)^soc (List.nth (List.nth sample 3) 2)^soc (List.nth (List.nth sample 3) 3)^soc (List.nth (List.nth sample 3) 4)^"\n"    );
	print_string ("The minimum value after "^soi 0^" is "^soi (minVal (objVal (alpToDec sample)) 1000)^" At "^soi ( minInx (objVal (alpToDec sample)) (objVal (alpToDec sample)) 0 10000)^"\n");
	
	let ans = gAlgo sample 0 minOVal in
	
	
	Printf.printf "%d\n",ans;
;;