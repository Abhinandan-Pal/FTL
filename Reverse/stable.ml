(*VARIOUS TYPES NEEDED*)
type feature = string
let compare = compare
let equal f1 f2 = compare f1 f2 = 0

module Feat = struct
  type t = feature

  let compare = compare
end

module FSet = Set.Make(Feat)
module FMap = Map.Make(Feat)




type var = int
let compare = compare
let equal m n = compare m n = 0

module Var = struct
  type t = var

  let compare = compare
end

module VarMap = Map.Make(Var)




type node = { var: var;
			 feat: node FMap.t;
			 abFeat: FSet.t}


let empty_node ():node = {var = 0;feat= FMap.empty;abFeat=FSet.empty}
let empty_node var = {var = var;feat= FMap.empty;abFeat=FSet.empty}

type atom =
  | Eq of var * var
  | Eqf of var * feature list * var
  | Feat of var * feature * var
  | Abs of var * feature
  | Fen of var * feature list
  | Sim of var * feature list * var

type var_map_t = node VarMap.t

type clause = atom list


(*VARIOUS FUNCTIONS NEEDED*)

let find_node var_map v1 = 
	let a = VarMap.find_opt v1 var_map in
	match a with
	| None -> let emt_n = empty_node v1 in
			  let var_map = VarMap.add v1 emt_n var_map in
			  (emt_n,var_map)
	| Some nod -> (nod,var_map)


let add_feat_to_node atom  var_map = 
	match atom with
	| Feat(v1,f,v2) -> let (v1_node,var_map) = find_node var_map v1 in
					   let (v2_node,var_map) = find_node var_map v2 in

					   if(FMap.find_opt f v1_node.feat = Some v1_node) then failwith "Crash: add_feat_to_node"

					    else (VarMap.add v1 {v1_node with feat = FMap.add f v2_node v1_node.feat} var_map)
	|_ -> failwith "add_feat_to_node is only for Feat"


let add_abs_to_node atom var_map = 
	match atom with
	| Abs (v1,f) -> let (v1_node,var_map) = find_node var_map v1 in				
					if(FMap.find_opt f v1_node.feat <> None) then failwith "Crash: add_abs_to_node"
					else (VarMap.add v1 {v1_node with abFeat = FSet.add f v1_node.abFeat} var_map)
	|_ -> failwith "add_abs_to_node is only for Abs"


let feat_map_combine v1_node v2_node f =
		let v1_f = v1_node.feat in
		let v2_f = v2_node.feat in
		let f_1 = FSet.elements f in
		let rec helper v1_f v2_f f_2 = 
			match f_2 with 
			|[] -> (v1_f,v2_f)
			|f::t -> match ((FMap.find_opt f v1_f),(FMap.find_opt f v1_f)) with
					|(None,None)-> helper v1_f v2_f t 

					|(Some n,None) -> let v2_f = FMap.add f n v2_f in
							 		  helper v1_f v2_f t

					|(None,Some n) -> let v1_f = FMap.add f n v1_f in
							 		  helper v1_f v2_f t

					|(Some n1,Some n2) -> if(n1 = n2) 
							 			then helper v1_f v2_f t
							 			else failwith "Crash:feat_map_combine"
		in
		let(v1_f,v2_f) = helper v1_f v2_f f_1 in
		let v1_node = {v1_node with feat = v1_f} in
		let v2_node = {v2_node with feat = v2_f} in
		(v1_node,v2_node)



let add_equal_to_node atom var_map = 
	match atom with
	| Eqf(v1,fl,v2) -> let (v1_node,var_map) = find_node var_map v1 in
					  let (v2_node,var_map) = find_node var_map v2 in

					  let fl = FSet.of_list fl in

					  let v1_node = {v1_node with abFeat = FSet.union v1_node.abFeat (FSet.inter fl v2_node.abFeat)} in
					  let v2_node = {v1_node with abFeat = FSet.union v1_node.abFeat (FSet.inter fl v2_node.abFeat)} in
									 
					  let (v1_node,v2_node) = feat_map_combine v1_node v2_node fl in (*Exception handelling has to be done*)

					  let var_map = VarMap.add v1 v1_node var_map in
					  let var_map = VarMap.add v2 v2_node var_map in
					  var_map
	|_ -> failwith "add_equal_to_node is only for Equal"


let rec clause_feat_abs (clau:clause) var_map =
		match clau with 
		|[] -> var_map
		|Feat(v1,f,v2)::t -> clause_feat_abs t (add_feat_to_node (Feat(v1,f,v2)) var_map)
		|Abs (v1,f)::t -> clause_feat_abs t (add_abs_to_node (Abs(v1,f)) var_map)
		| _ :: t -> clause_feat_abs t var_map




let node_display {feat = feat_ ;abFeat = abFeat_ } : unit = 
		
		let feat_ = FMap.bindings feat_ in
		let rec feat_display feat_ = 
			match feat_ with 
			|[] -> ()
			|(f_1,n_1)::t -> Format.printf "[%s --> %d]\t" f_1 n_1.var;
											 feat_display t 
		in 
		
		let abFeat_ = FSet.elements abFeat_ in
		let rec abFeat_display abFeat_ = 
			match abFeat_ with 
			|[] -> ()
			| h::t -> Format.printf "[%s]\t" h;
								abFeat_display t
		in 

		Format.printf "Features:\n";
		feat_display feat_;
		Format.printf "\nAbsent Features:\n\n";
		abFeat_display abFeat_

let var_map_display var_map = 
		let var_map = VarMap.bindings var_map in
		let rec helper var_map = 
			match var_map with 
			|[] -> ()
			|(v_1,n_1)::t -> Format.printf "NODE(VAR) : %d\n" v_1;
										node_display n_1;
										helper t
		in 
		helper var_map













(*EXAMPLE-TEST*)

let v1:var = 1
let v2:var = 2
let v3:var = 3
let v4:var = 4

let f1:feature = "a"
let f2:feature = "b"
let f3:feature = "c"
let f4:feature = "d"

let (clau_1:clause) = [Feat(v1,f1,v2);Feat(v1,f2,v4);Feat(v1,f3,v3)]

let (var_map:var_map_t) = VarMap.empty


let var_map = clause_feat clau_1 var_map

let () = var_map_display var_map