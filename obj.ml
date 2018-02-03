(* An implementation of TSP Solution using Genetic Algorithm********************************************************************************
	Author: Piyush Kaushik *****************************************************************************************************************
	Date: 20 July 2014**********************************************************************************************************************
*)

(* The Following are values used through out the program. Here 'n' represents the number of candidates in one generation, 'g' is the number 
	of bits in each candidate solution, and finally 'r' is the termination condition, which is the total number of generations required in 
	the algorithm. All three can be changed according to your need and input data.
*)

let n = 5;;
let g = 4;;
let r = 10;;
let minOVal = 1000000;;
let obj = [[0;6;9;20;10];[6;0;11;30;40];[9;11;0;25;14];[20;30;25;0;8];[10;40;14;8;0]];;

let objTrv x y = List.nth (List.nth obj x) y;;

let rec objFun lst num point = 
	if point = (n-1) then
		num + objTrv (List.nth lst 0) (List.nth lst (n-1))
	else
		objFun lst (num + objTrv (List.nth lst point) (List.nth lst (point + 1))) (point + 1);
;;