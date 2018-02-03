(* An implementation of TSP Solution using Genetic Algorithm********************************************************************************
	Author: Piyush Kaushik *****************************************************************************************************************
	Date: 20 July 2014**********************************************************************************************************************
*)

(* Utility Functions ***********************************************************************************************************************
	these are the utility functions used in the program, some are just for simplifications and others are for calculation purposes. ********
*)

let soi x = string_of_int x
let soc x = String.make 1 x
let foi x = float_of_int x
let iof x = int_of_float x

let rec sumList lst =
	match lst with
	[] -> 0
	|h :: t -> h + sumList t;
;;


let avgVal lst = 
	let sum = sumList lst in
	sum / (List.length lst);
;;

let alpVal n =
	match n with
	'A' -> 0
	|'B' -> 1
	|'C' -> 2
	|'D' -> 3
	|'E' -> 4
	|'F' -> 5
	|'G' -> 6
	|'H' -> 7
	|'I' -> 8;
;;

let rec alpLVal lst = 
	match lst with
	[] -> []
	|h::t -> (alpVal h) :: (alpLVal t);
;;

let rec alpToDec sample = 
	match sample with
	[] -> []
	|h::t -> (alpLVal h) :: (alpToDec t);
;;

let rec minVal lst temp = 
	match lst with
	[] -> temp
	|h :: t -> if h < temp then minVal t h else minVal t temp;
;;

let rec minInx lst clst num flag=
	let min = minVal clst flag in
	match lst with
	[] -> -10
	|h :: t -> if h = min then num else minInx t clst (num+1) flag;
	
;;


let rec maxInx lst clst num temp =
	match lst with 
	[] -> temp
	|h :: t -> if h > (List.nth clst temp) then (maxInx t clst (num + 1) num) else (maxInx t clst (num + 1) temp);
;;
	
let rec getPart lst point num =
	if num = point then []
	else
		match lst with 
			[] -> []
			|h :: t -> h :: getPart t point (num+1);
;;

let rec findList lst num = 
	match lst with 
		[] -> false
		|h :: t -> if h = num then true else findList t num;
;;

let rec getRest lst plst =
	match lst with
		[] -> []
		|h :: t -> if (findList plst h = false) then h :: (getRest t plst) else (getRest t plst);
;;