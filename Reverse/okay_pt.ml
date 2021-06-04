module FTL = struct
	
	(*---------Defining all the neccessary types----------*)
	
	(*path element: atomic sub-parts of a path which
	 include '.',"..",file/directory*)
	type path_ele = Curr | Back | Dir of string

	
	(*path: List of path elements , It can either be
	 Absolute or Relative as indicated by constuctor*)
	type path = Abs of path_ele list | Rel of path_ele list

	
	(*feature: Name for path between two nodes of a feature tree*)
	type feature = string


	(*Feature tree: A recursive type where each Node has a Name
	 and n no of features each leading to different feature tree.
	 A feature Tree can also be empty.
	 Assumption : features are unique for a Name*)
	type f_tree = Empty
			 |Node of string * (feature * f_tree) list


	(*Variable: It can store the Name of variable, its value(Feature tree) or Empty.
	 Empty refers to top arrow as in paper*)
	type var = Name of string| Value of f_tree |Empty 
	
	
	(*Equality Set: Set over which two variables can be equal.
	 It can be All feature or the given list or all but the given list*)
	type equality_set = |All|List of feature list|C_list of feature list
	
	

	(*Logic: Represent various structures of First order logic.*)
	type logic = 
				|Branch of var * feature * var 			(*x[f]y*)
				|Equality of var *  equality_set * var 	(*x =F y*)
				|True_ 
				|False_
				|Neg of logic							 (* -theta *)
				|And of logic * logic 					 (*theta AND phi*)
				|Or of logic * logic 				     (*theta OR phi*)
				|Exist of var * logic 					 (*Existence of x in theta*)
				|Uni of var * logic 					 (*Universality of x in theta*)
				|Bracket of logic 						 (*(theta)*)
	
	
	(*---------Defining all the neccessary functions to handel the types----------*)
	
	
	(*convert_to_path: Convert a path in string into a list of path elements. Seperate thring on the basis of '/'.
	 And then represent each part with required path_ele constructor*)
	
	let convert_to_path (str:string) : path_ele list = 
		let str2 = String.split_on_char '/' str in 
		let rec helper_1 = function
			|[] -> []
			|[""] -> []
			|"."::t -> Curr::(helper_1 t)
			|".."::t -> Back::(helper_1 t)
			|h::t -> Dir h ::(helper_1 t)
		in 
		helper_1 str2


	(*UNIMPLEMENTED : Convert list of path element to path*)


	(*path_to_list : Convert a path to list of path elements*)
	let path_to_list (pth:path): path_ele list =
		match pth with
		|Abs h -> h 
		|Rel h -> h

	(*convert_from_path: Converts a list of path elements to String in format we are used to see paths in*)
	let convert_from_path (pth :path_ele list) : string =
		let rec helper_2 = function
			| [] ->""
			| Curr::t-> "./"^helper_2 t
			| Back::t-> "../"^helper_2 t
			| Dir h::t-> h^"/"^helper_2 t
		in 
		helper_2 pth

	(*concat_path : contact a Absolute path(cwd) with a Relative path to form a Absolute path*)
	let concat_path (p1:path) (p2:path) : path_ele list =
					match(p1,p2) with
					|(Abs cwd ,Rel r2) -> (cwd@r2)
					|(_,Abs r2) -> r2
					|(_,_) -> failwith " cwd has to be absolute"

	(*logic_to_str : Return a Mathematical representation of a type Logic data . Use print_string *)
	let rec logic_to_str (log:logic):string = 
		match log with
		| Branch (x,f,y) -> begin
							match (x,y) with 
							|(Name x1,Name y1) -> x1^"["^f^"]"^y1
							|(Name x1,Empty) -> x1^"["^f^"]↑"
							|(_,_) -> failwith "Branches of this type are not to occur."
							end
		| Equality (x,f_l,y) -> begin
							match (x,f_l,y) with 
							|(Name x1,List f_ll,Name y1) -> x1^"=["^ (List.fold_left (fun y x -> y^","^x) "" f_ll) ^"]"^y1
							|(Name x1,All,Name y1) -> x1^"=*"^y1
							|(Name x1,C_list f_lc,Name y1) -> x1^"=ᶜ["^ (List.fold_left (fun y x -> y^","^x) "" f_lc) ^"]"^y1
							|(_,_,_) -> failwith "Equality of given type are not to occur."
							end 
		| True_ -> "⊤"
		| False_ -> "⊥"
		| Neg (l) -> "¬("^ (logic_to_str l) ^" )"
		| And (l1,l2) -> "" ^ (logic_to_str l1) ^ " ∧ " ^ (logic_to_str l2) ^ ""
		| Or (l1,l2) -> "" ^ (logic_to_str l1) ^ " ∨ " ^ (logic_to_str l2) ^ ""
		| Exist (v,l) -> begin
							match v with 
							|Name x1 -> "∃"^x1^"( "^ (logic_to_str l) ^ " )"
							|_ -> failwith "Variable can't be empty or or have a value"
						 end 
		| Uni (v,l) -> begin
							match v with 
							|Name x1 -> "∀"^x1^"( "^ (logic_to_str l) ^ " )"
							|_ -> failwith "Variable can't be empty or or have a value"
						 end 
	
		| Bracket (l) -> "( "^(logic_to_str l)^" )"



	(*---------Defining all the Functions as in chapter-3----------*)

	(*---RESLOVE SECTION---*)

	(*reslove_s : page 51*)
	(*In both cases we are able to use list as list removes the one last added(head).
	 And when path is created it is conde recursively from the thus end of path added first*)
	
	let rec resolve_s (x:var) (pi: var list) (q:path_ele list) (z:var) (num:int) =
		match q with
		| [] -> Equality (x,All, z)
		| Curr::t -> (resolve_s x pi t z num)
		| Back:: t ->begin
			match pi with
				| []-> (resolve_s x pi t z num)
				| h2::t2 -> (resolve_s h2 t2 t z num)
			end
		| Dir h::t -> let (y:var) = Name ("y"^ string_of_int num) in
						Exist (y, ( And (Branch (x, h, y), (resolve_s y (x::pi) t z (num+1)) )  ) )


	(*resolve : page 51*)
	let resolve (r: var) (cwd: path) (p:path) (z:var) = 
		match p with
		| Abs q -> (resolve_s r [] q z 1)
		| Rel q -> (resolve_s r [] (concat_path cwd p) z 1)
	


	(*---SIMILAR SECTION---*)

	(*normalize : 52*)
	let normalize (cwd: path) (q:path_ele list) : path =
	match cwd with 
	| Abs p ->
			   let cwd2 = List.rev p in
			   let rec helper_3 (cwd_h: path_ele list) (q_h:path_ele list) = 
	   			  match q_h with
	   			  | [] -> cwd_h
	   			  | Curr::t -> (helper_3 cwd_h t)
				  | Back:: t ->begin
				  	match cwd_h with
				  		| []-> (helper_3 [] t)
				  		| h2::t2 -> (helper_3 t2 t)
				  	end
				  | Dir h::t -> (helper_3 (Dir h::cwd_h) t)

				in
				Abs (List.rev (helper_3 cwd2 q))

	| Rel _ -> failwith "cwd can be relative"


	(*similar_n : 52*)
	let rec similar_n (x:var) (x_1:var) (q: path_ele list) (z:var) (z_1:var) (num:int): logic =
		match q with 
		| [] -> And(Equality (x,All,z),Equality (x_1,All,z_1))
		| Dir h::t -> let (y:var) = Name ("y"^ string_of_int num) in
				  let (y_1:var) = Name ("y'"^ string_of_int num) in
				  Exist(y,(Exist (y_1,And(Branch(x,h,y),And(Branch(x_1,h,y_1),And(Equality(x,C_list [h],x_1),(similar_n y y_1 t z z_1 (num+1))))))))
		| _ -> failwith "Path not Normalized"


	(* similar : 52*)
	let similar (r:var) (r_1:var) (cwd:path) (p:path) (z:var) (z_1:var): logic =
		match p with 
		| Abs q -> similar_n r r_1 (path_to_list (normalize (Abs []) q)) z z_1 1
		| Rel q -> similar_n r r_1 (path_to_list (normalize cwd q)) z z_1 1


	(*---NORESLOVE SECTION---*)

	(* noresolve_s : 53*)
	let rec noresolve_s (x:var) (pi: var list) (q:path_ele list) (num:int) =
		match q with
		| [] -> False_
		| Curr::t -> (noresolve_s x pi t num)
		| Back:: t ->begin
			match pi with
				| []-> (noresolve_s x pi t num)
				| h2::t2 -> (noresolve_s h2 t2 t num)
			end
		| Dir h::t -> let (y:var) = Name ("y"^ string_of_int num) in
						Bracket (Or( Branch (x,h,Empty) , Exist(y ,  And( Branch(x,h,y) , (noresolve_s y (x::pi) t (num+1)) )) ))


	(* noresolve : 53*)
	let noresolve (r: var) (cwd: path) (p:path) = 
		match p with
		| Abs q -> (noresolve_s r [] q 1)
		| Rel q -> (noresolve_s r [] (concat_path cwd p) 1)

end


open FTL

let x = "./Internships/../Feature Tree Logic/./OCAML-learn/"
let y = convert_to_path x
let z = convert_from_path y
let path_1 = Abs y

let x_var = Name "x"
let z_var = Name "z"
let cwd = Abs []

let resolved_path = resolve x_var cwd path_1 z_var

let normalized_path = convert_from_path(path_to_list (normalize cwd y))

let r = Name "r"
let r_1 = Name "r'"
let z = Name "z"
let z_1 = Name "z'"

let similar_log = similar r r_1 cwd path_1 z z_1 

let noresolved_path = noresolve x_var cwd path_1

let prn ()= print_string ("\n\nInput Path : "^(x)^"\n");
			print_string ("\nNormalized Path : "^(normalized_path)^"\n");
			print_string ("\nResolved : "^(logic_to_str resolved_path)^"\n");
			print_string ("\nSimiliar : "^(logic_to_str similar_log)^"\n");
			print_string ("\nNo Resolved Path : "^(logic_to_str noresolved_path)^"\n\n")

let _ = prn ()


(*Note : try adding a bracket outside every recursive call of resolve and similar just like in noresolve*)
(*Replace List be set where needed*)

(*UNIMPLEMENTED : Convert to PNF*)