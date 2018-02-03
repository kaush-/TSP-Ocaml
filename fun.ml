(* An implementation of TSP Solution using Genetic Algorithm********************************************************************************
	Author: Piyush Kaushik *****************************************************************************************************************
	Date: 20 July 2014**********************************************************************************************************************
*)


(* Selection Code ***************************************************************************************************************************
	This is the most complex part of this code, It implements the selection procedure of our genetic algorithm. This is done by, first calculating
	the fitness of each candidate, then normalizing it and after that replacing the candidate solution with MAX normalization value with the MIN
	value candidate. ***************************************************************************************************************************
*)	

let rec innerFitVal lst favg =
	match lst with 
	[] -> []
	|h::t ->  try (((foi h) /. favg)  :: (innerFitVal t favg)) with Division_by_zero -> [];
;;
	
let fitVal lst =
	let avg = (avgVal lst) in 
	let favg = foi avg in
	innerFitVal lst favg;
;;

let rec objVal sample =
	match sample with 
	[] -> []
	|h::t -> (objFun h 0 0) :: (objVal t);
;;

let rec innerReplace sample csample min max =
	match sample with 
	[] -> []
	|h :: t -> if h = (List.nth csample min) then ((List.nth csample max) :: (innerReplace t csample min max)) else (h :: (innerReplace t csample min max));
;;	

let replace sample min max =
	innerReplace sample sample min max;
;;

let select sample lst =
	let min = minInx lst lst 0 1000.0  in
	let max = maxInx lst lst 0 0  in
	if min <> -1 then
		replace sample min max
	else
		sample;
;;

let rec selection sample =
      select sample (fitVal (objVal (alpToDec sample)));
;;

(* End of Selection *)

(* CrossOver Code ****************************************************************************************************************************
	Here two parents are selected to mate and create a new child. I am using a random point to perform the crossover 
	instead of going with the traditional fixed point mating. ********************************************************************************
*)


let mate lst prt cpoint =
	print_string (""^soc (List.nth lst 0)^soc (List.nth lst 1)^soc (List.nth lst 2)^soc (List.nth lst 3)^soc (List.nth lst 4)^" is mating with "^soc (List.nth prt 0)^soc (List.nth prt 1)^soc (List.nth prt 2)^soc (List.nth prt 3)^soc (List.nth prt 4)^" at "^soi cpoint^"\n");
	let fpart = (getPart lst cpoint 0) in 
	(List.append fpart (getRest prt fpart));
;;

let rec crossover sample csample =
	match sample with
	[] -> []
	|h :: t -> (mate h (List.nth csample (Random.int(g))) (Random.int(n-1))) :: (crossover t csample);
;;

(* End of CrossOver *)

(* Mutation Code ****************************************************************************************************************************
	These two functions takes the input population and mutate each bit with a probability of 1/n ********************************************
*)
let rec innerMutate lst fval nval = 
	match lst with
	[] -> []
	|h :: t -> if h = fval then nval :: innerMutate t fval nval else if h = nval then fval :: innerMutate t fval nval else h :: innerMutate t fval nval;
;;


let rec mutate lst num = 
	let x = Random.int(n) in
	if num = n then lst
	else if x = (n-1) then mutate (innerMutate lst (List.nth lst num) (List.nth lst (Random.int(n-1)))) (num + 1)
	else mutate lst (num + 1);
;;

let rec mutation sample =
	match sample with
	[] -> []
	|h :: t -> (mutate h 0) :: mutation t;
;;

(* End of Mutation *)

let rec gProc sample gen minOVal =
	let newSample = mutation (crossover (selection sample) sample) in
	print_string (""^soc (List.nth (List.nth sample 0) 0)^soc (List.nth (List.nth sample 0) 1)^soc (List.nth (List.nth sample 0) 2)^soc (List.nth (List.nth sample 0) 3)^soc (List.nth (List.nth sample 0) 4)^"\n"^soc (List.nth (List.nth sample 1) 0)^soc (List.nth (List.nth sample 1) 1)^soc (List.nth (List.nth sample 1) 2)^soc (List.nth (List.nth sample 1) 3)^soc (List.nth (List.nth sample 1) 4)^"\n"^soc (List.nth (List.nth sample 2) 0)^soc (List.nth (List.nth sample 2) 1)^soc (List.nth (List.nth sample 2) 2)^soc (List.nth (List.nth sample 2) 3)^soc (List.nth (List.nth sample 2) 4)^"\n"^soc (List.nth (List.nth sample 3) 0)^soc (List.nth (List.nth sample 3) 1)^soc (List.nth (List.nth sample 3) 2)^soc (List.nth (List.nth sample 3) 3)^soc (List.nth (List.nth sample 3) 4)^"\n"    );
	if minOVal >= (minVal (objVal (alpToDec sample)) 1000) then 
		begin
			print_string ("The minimum value after "^soi (gen + 1)^" is "^soi (minVal (objVal (alpToDec sample)) 1000)^" At "^soi ( minInx (objVal (alpToDec sample)) (objVal (alpToDec sample)) 0 10000)^"\n");
			gAlgo newSample (gen+1) (minVal (objVal (alpToDec sample)) 1000)
		end
	else
		begin
			print_string ("The minimum value after "^soi (gen + 1)^" is "^soi (minVal (objVal (alpToDec sample)) 1000)^" At "^soi ( minInx (objVal (alpToDec sample)) (objVal (alpToDec sample)) 0 10000)^"\n But the Overall Minimum Value is "^soi(minOVal)^"\n");
			gAlgo newSample (gen+1) minOVal
		end;

and gAlgo sample gen minOVal = 
	if gen = r then
		true
	else
		gProc sample gen minOVal;
;;
