type feature = string

type var = int
type node = {Var:var;
			 Feat: node FMap
			 AbFeat: FSet}
(*Better stucture: let atom nbe as in atom.ml and map from node to Var.t*)
type atom =
  | Eq of var * var
  | Feat of var * feature * var
  | Abs of var * feature
  | Fen of var * (feature list)
  | Sim of var * (feature list) * var
 
let all_var: node list = []
let empty_node ():node = {feat=[];abFeat=[]}

(*find_node : finds a node from a map given the vars if not presnt then creates an empty*)

let find_node ~var_map:vm v1 = 
	let a = VarMap.find_opt v1 vm in
	match a with
	| None -> let emt_n = empty_node ()
						let vm = VarMap.add v1 emt_n var_map in
						(emt_n,vm)
	| Some nod -> (nod,vm)



let add_feat_to_node ~atom:a  var_map = 
	match atom with
	| Feat(v1,f,v2) -> let (v1_node,var_map) = find_node var_map v1 in
										 let (v2_node,var_map) = find_node var_map v2 in

										 if(FMap.find_opt f v1_node.feat = Some v1_node) then failwith "Crash: add_feat_to_node"

					   				 else (VarMap.add v1 {v1_node with feat = FMap.add f v2_node v1_node.feat} var_map)



let add_abs_to_node ~atom:a var_map = 
	match atom with
	| Abs (v1,f) -> let (v1_node,var_map) = find_node var_map v1 in
									
									if(FMap.find_opt f v1_node.feat <> None) then failwith "Crash: add_abs_to_node"
					   			else (VarMap.add v1 {v1_node with absFeat = FSet.add f v1_node.absFeat} var_map)


let feat_map_combine v1_node v2_node F =
		v1_f = v1_node.feat
		v2_f = v2_node.feat
		let F_1 = FSet.elements F in
		let helper v1_f v2_f F_2 = 
			match F_2 with 
			|[] -> (v1_f,v2_f)
			|f::t -> match ((FSet.find_opt f v1_f),(FSet.find_opt f v1_f)) with
							 |(None,None)-> helper v1_f v2_f t 

							 |(Some n,None) -> v2_f = FSet.add f v2_f in
							 									 helper v1_f v2_f t

							 |(None,Some n) -> v1_f = FSet.add f v1_f in
							 									 helper v1_f v2_f t

							 |(Some n1,Some n2) -> if(n1 = n2) 
							 											 then helper v1_f v2_f t
							 											 else failwith "Crash:feat_map_combine"
		in
		let(v1_f,v2_f) = helper v1_f v2_f F_1 in
		let v1_node = (v1_node with feat = v1_f) in
		let v2_node = (v2_node with feat = v2_f) in
		(v1_node,v2_node)



let add_equal_to_node ~atom:a var_map = 
	match atom with
	| Eqf(v1,F,v2) -> let (v1_node,var_map) = find_node var_map v1 in
									 let (v2_node,var_map) = find_node var_map v2 in

									 let F = FSet.of_List F in

									 v1_node = (v1_node with absFeat = FSet.union v1_node.absFeat (FSet.inter F.absFeat v2_node.absFeat)) in
									 v2_node = (v1_node with absFeat = FSet.union v1_node.absFeat (FSet.inter F.absFeat v2_node.absFeat)) in
									 
									 (v1_node,v2_node) = feat_map_combine v1_node v2_node F in (*Exception handelling has to be done*)

					   			 let var_map = VarMap.add v1 v1_node var_map in
					   			 let var_map = VarMap.add v2 v2_node var_map in
					   			 var_map

type clause = atom list

let rec clause_feat clau:clause var_map =
	match clau with 
	|[] -> var_map
	|h::t -> clause_feat t (add_feat_to_node h var_map)

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
		Format.printf "Absent Features:\n";
		abFeat_display abFeat_


let var_map_display var_map = 
		let var_map = VarMap.bindings var_map in
		let rec helper var_map = 
			match var_map with 
			|[] -> ()
			|(v_1,n_1)::t -> Format.printf "NODE(VAR) : %d" v_1;
										node_display n_1;
										helper t
		in 
		helper var_map