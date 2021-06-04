(*This section is under development, Thus some parts might not function properly*)
(*What if we think of a node as Absent and map feature to its var when ever Ab(x,f)*)
(*CHECK IMPORTANT COMMENT*)
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
module VSet = Set.Make(Var)


type node = { var_l: var list;
			 feat: var FMap.t;
			 equality: (FSet.t*var) list; 
			 sim: (FSet.t*var) list
			 fen : FSet.t; (*empty signifies no Fen specified so all allowed*)
			 }

let empty_node ():node = {var_l = [];feat = FMap.empty;equality = [],sim = [],fen = FSet.empty}
let empty_node v:node = {var_l = [v];feat = FMap.empty;equality = [],sim = [],fen = FSet.empty}


type atom =
  | Eq of var * var
  | Eqf of var * feature list * var
  | Feat of var * feature * var
  | Abs of var * feature
  | Fen of var * feature list
  | Sim of var * feature list * var

type var_map_type = node VarMap.t

type clause = atom list

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i    





(*VARIOUS FUNCTIONS NEEDED*)


let node_display {feat = feat_ ;equality= equality_;sim= sim_;fen = fen_} : unit = 
		
		let feat_ = FMap.bindings feat_ in
		let rec feat_display feat_ = 
			match feat_ with 
			|[] -> ()
			|(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" f_1 v_1;
											 feat_display t 
		in 
		
		let fen_ = FSet.elements fen_ in
		let rec fen_display fen_ = 
			match fen_ with 
			|[] -> ()
			| h::t -> Format.printf "[%s]\t" h;
								fen_display t
		in 

		let rec equality_display equality_ = 
			match equality_ with 
			|[] -> ()
			|(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" "F" v_1; (*ADD FOR FSET*)
								equality_display t
		in

		let rec sim_display sim_ = 
			match sim_ with 
			|[] -> ()
			|(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" "F" v_1; (*ADD FOR FSET*)
								sim_display t
		in

		Format.printf "Features:\n";
		feat_display feat_;
		Format.printf "\nFen Features:\n";
		fen_display fen_;
		Format.printf "\nEquality:\n";
		equality_display equality_
		Format.printf "\nSimilarity:\n";
		sim_display sim_


let var_map_display var_map = 
		let var_map = VarMap.bindings var_map in
		let rec helper var_map = 
			match var_map with 
			|[] -> ()
			|(v_1,n_1)::t -> Format.printf "\n\t\tNODE(VAR) : %d\n" v_1;
										node_display n_1;
										helper t
		in 
		helper var_map




let rec create_empty_var_map clause var_map (fBigSet:FSet.t) = 
	match clause with 
	|[] -> var_map
	|Feat(v1,f,v2)::t -> let var_map = VarMap.add v1 (empty_node v1) var_map in
											 let var_map = VarMap.add v2 (empty_node v2) var_map in
											 let fBigSet = FSet.add f fBigSet in
											 create_empty_var_map t var_map fBigSet

	|Abs (v1,f)::t -> let var_map = VarMap.add v1 (empty_node v1) var_map in
										let fBigSet = FSet.add f fBigSet in
										create_empty_var_map t var_map fBigSet

	|Eqf (v1,fl,v2)::t ->let var_map = VarMap.add v1 (empty_node v1) var_map in
										let var_map = VarMap.add v2 (empty_node v2) var_map in
										let fBigSet = FSet.union (FSet.of_list fl) fBigSet
										create_empty_var_map t var_map fBigSet

	| Eq(v1,v2)::t -> let var_map = VarMap.add v1 (empty_node v1) var_map in
										let var_map = VarMap.add v2 (empty_node v2) var_map in
										create_empty_var_map t var_map fBigSet

	|Sim (v1,fl,v2)::t ->let var_map = VarMap.add v1 (empty_node v1) var_map in
										let var_map = VarMap.add v2 (empty_node v2) var_map in
										let fBigSet = FSet.union (FSet.of_list fl) fBigSet
										create_empty_var_map t var_map fBigSet

	|Fen(v1,fl)::t -> let var_map = VarMap.add v1 (empty_node v1) var_map in
										let fBigSet = FSet.add f fBigSet in
										create_empty_var_map t var_map fBigSet


(*Creates an empty node if not found*)
let find_node_c var_map v1 = 
	let a = VarMap.find_opt v1 var_map in
	match a with
	| None -> let emt_n = empty_node v1 in
			  let var_map = VarMap.add v1 emt_n var_map in
			  (emt_n,var_map)
	| Some nod -> (nod,var_map)

let find_node var_map v1 = 
	let a = VarMap.find_opt v1 var_map in
	match a with
	| None -> failwith "Could not find Var"
	| Some nod -> nod 

let add_feat_to_node atom var_map = 
	match atom with
	| Feat(v1,f,v2) -> let v1_node = find_node var_map v1 in
							if (v1_node.feat.find_opt f = Some 0) then failwith "Clash: tring to create x[f]y when x[f]abs exists"
					    else if (not(FSet.is_empty v1_node.fen) && not(FSet.mem f v1_node.fen)) then failwith "Clash: tring to create x[f]y when x[F] exists and f does not belong to F"
					   else 
					   let new_node = {v1_node with feat = FMap.add f v2 v1_node.feat} in
					   let rec helper var_map vl =
								match vl with 
								|[]-> var_map 
								|v1::t -> helper (VarMap.add v1 new_node var_map) t
						 in
						 (helper var_map new_node.var_l)
	|_ -> failwith "add_feat_to_node is only for Feat"


let add_abs_to_node atom var_map = 
	match atom with
	| Abs (v1,f) -> let v1_node = find_node var_map v1 in	
					let absent_var = 0 in	
					if ((v1_node.feat.mem f)&&(v1_node.feat.mem f <> Some 0)) then failwith "Clash: tring to create x[f]abs when x[f]y exists"		
					else
					let new_node = {v1_node with feat = FMap.add f absent_var v1_node.feat} in
					   let rec helper var_map vl =
								match vl with 
								|[]-> var_map 
								|v1::t -> helper (VarMap.add v1 new_node var_map) t
						 in
						 (helper var_map new_node.var_l)
					(*All mappings from f to 0 are considered absent*)
	|_ -> failwith "add_abs_to_node is only for Abs"

let add_equal_to_node atom var_map = 
	match atom with
	| Eqf (v1,fl,v2) -> let v1_node = find_node var_map v1 in
											let v2_node = find_node var_map v2 in
						let fl_v = (FSet.of_list fl,v2) in 				
						let var_map = (VarMap.add v1 {v1_node with equality = fl_v :: v1_node.equality} var_map) in
						let fl_v = (FSet.of_list fl,v1) in 	
						(VarMap.add v2 {v2_node with equality = fl_v :: v2_node.equality} var_map)
	|_ -> failwith "add_equal_to_node is only for Eqf"

let add_sim_to_node atom var_map = 
	match atom with
	| Sim (v1,fl,v2) -> let v1_node = find_node var_map v1 in
											let v2_node = find_node var_map v2 in
						let fl_v = (FSet.of_list fl,v2) in 				
						let var_map = (VarMap.add v1 {v1_node with sim = fl_v :: v1_node.sim} var_map) in
						let fl_v = (FSet.of_list fl,v1) in 	
						(VarMap.add v2 {v2_node with sim = fl_v :: v2_node.sim} var_map)
	|_ -> failwith "add_equal_to_node is only for Eqf"

let add_fen_to_node atom var_map = 
	match atom with
	| Abs (v1,fl) -> let v1_node = find_node var_map v1 in	
					let fl = FSet.of_list fl in
					let fen_new = (if(FSet.is_empty v1_node.fen)then fl else (FSet.inter fl v1_node.fen)) in
					(VarMap.add v1 {v1_node with fen = fen_new} var_map)
	|_ -> failwith "add_abs_to_node is only for Abs"




let node_union (n1:node) (n2:node):node= 
	let nf_abFeat = FSet.union n1.abFeat n2.abFeat in
	let nf_equality = n1.equality @ n2.equality in
	let nf_var_l = n1.var_l @ n2.var_l in
	let merge k n1 n2 = if(n1=n2) then Some n1
						else failwith "Crash:feat_map_combine_ALL";in
	
	let nf_feat = FMap.union merge n1.feat n2.feat in

	({var_l = nf_var_l;feat = nf_feat; abFeat = nf_abFeat; equality = nf_equality})
	


let add_equal_to_node_ALL atom var_map = 
	match atom with
	| Eq (v1,v2) -> let v1_node = find_node var_map v1 in
					let v2_node = find_node var_map v2 in
					let new_node = node_union v1_node v2_node in 				
					let var_map = VarMap.add v1 new_node var_map in
					VarMap.add v2 new_node var_map

	|_ -> failwith "add_equal_to_node is only for Eq"

let rec clause_feat_abs (clau:clause) var_map =
		match clau with 
		|[] -> var_map
		|Feat(v1,f,v2)::t -> clause_feat_abs t (add_feat_to_node (Feat(v1,f,v2)) var_map)
		|Abs (v1,f)::t -> clause_feat_abs t (add_abs_to_node (Abs(v1,f)) var_map)
		| _ :: t -> clause_feat_abs t var_map


let rec clause_eqf_eq (clau:clause) var_map =
		match clau with 
		|[] -> var_map
		|Eqf(v1,fl,v2)::t -> clause_eqf_eq t (add_equal_to_node (Eqf(v1,fl,v2)) var_map)
		| _ :: t -> clause_eqf_eq t var_map

let invertRes (x:bool) (y:bool) = if(y)then (not x) else x
(*For equality f needs to exist in F and for sim no exist so use this*)

let rec find_feat_link_opt (n:node) (var_map) (f:feature) (vlist)=
	let vlist = vlist@n.var_l in
	match (FMap.find_opt f n.feat) with
	| Some x -> let var_map = add_feat_to_node (Abs((List.hd n.var_l),f,x)) in
							(var_map,x) 

	| None ->let rec helper1 l_l var_map (flip:bool) =
			  	 match l_l with
			  	 |[] -> (var_map,None)   
			  	 |(l,v)::t when (not (List.mem v vlist)) -> 
			  	 				let v_node = find_node var_map v in
			  	 				if (invertRes (FSet.mem f l) flip)then
			  	 						let (var_map,x) = (find_feat_link_opt v_node var_map f vlist) in 
			  	 				 		if(x <> None)then
			  	 				 				let var_map = add_feat_to_node (Abs(v,f,x)) in
													(var_map,Some x) 
											else helper1 t var_map flip
			  	 				else helper1 t var_map flip

			  	 	|_::t -> helper1 t var_map flip
			  	 in
			   let (var_map,x) = helper1 n.equality var_map false in
			   if(x = None) then (helper1 n.sim var_map true) (*try using sim*)
			 	 else (var_map,x)


(*Think about the clash condition where x =F y and x =G z and f belongs to both F,G but y[f] and g[f] are differnt*)
(*Think about abs*)
let disolve_node (n:node) var_map fBigSet= 
	let l = FSet.elements fBigSet
	let rec helper l var_map =
		match l with
		|[] -> var_map
		|f::t -> let (var_map,x) = find_feat_link_opt var_map f [] in
						 var_map
	in
	helper l var_map

let dissolve_all var_map =
	let var_map_l = VarMap.bindings var_map in
	let rec helper var_map_l var_map = 
		match var_map_l with 
		|[] -> var_map
		|(v_1,n_1)::t -> let var_map = disolve_node n_1 var_map in
									   helper t var_map
	in 
	helper var_map_l var_map



let not_Fen_transform atom var_map fBigSet =
	match atom with
	|Fen(v1,fl) -> let v1_node = find_node var_map v1 in
								 let all_f = FSet.elements (FSet.diff fBigSet fl) in 
								 let rec helper l var_map =
								 	match l with
								 	|[] -> 
								 	|f::t -> let (var_map,x) = find_feat_link_opt var_map f [] in
								 					 if(x <> None) then helper t var_map
								 					 else (var_map,fBigSet)
	in
	helper all_f var_map

	let helper2 var_map = if(FSet.is_empty v1_node.fen) then
														let (new_f:feature) = "GeneratedF"+fresh()
														let v_new = (VarMap.cardinal var_map) + 3 in
														let var_map = VarMap.add v_new (empty_node v_new) var_map in
														let var_map = add_feat_to_node (Feat (v1,new_f,v_new)) var_map  in
														let fBigSet = FSet.add new_f fBigSet in
														(var_map,fBigSet)
												else failwith "not_Fen_transform: [Clash]It is not possible to satisfy it"







let not_eq_sim_transform  atom var_map fBigSet =
	let all_f  = match atom with 
							 |Eqf(v1,fl,v2) -> fl
							 |Sim(v1,fl,v2) -> FSet.diff fBigSet fl
							 |_ -> failwith "node_not_eq_sim_transform is only for Eqf and Sim"


	let v1_node = find_node var_map v1 in
	let v2_node = find_node var_map v2 in
	let info = ref []
	let helper3 var_map info_s=
				match info_s with 
				|[] -> failwith "node_not_eq_transform : Could not find a way to satisfy it"
				|(f1,None,None)::t -> if ((FSet.is_empty v1_node.fen)||(FSet.mem f v1_node.fen)) then 
															let v_new = (VarMap.cardinal var_map) + 3 in
															let var_map = VarMap.add v_new (empty_node v_new) var_map in
															let var_map = add_feat_to_node (Feat (v1,f1,v_new)) var_map  in
															let var_map = add_abs_to_node (Abs (v2,f1)) var_map in
															var_map
													 else if((FSet.is_empty v2_node.fen)||(FSet.mem f v2_node.fen)) then 
															let v_new = (VarMap.cardinal var_map) + 3 in
															let var_map = VarMap.add v_new (empty_node v_new) var_map in
															let var_map = add_feat_to_node (Feat (v2,f1,v_new)) var_map  in
															let var_map = add_abs_to_node (Abs (v1,f1)) var_map in
															var_map
													  else helper3 var_map t
				in
	let helper2 var_map info_s =
				match info_s with 
				|[] -> helper3 var_map !info
				|(f1,Some v_1, None)::t -> var_map = add_abs_to_node (Abs (v2,f1)) var_map  in
															 var_map
				|(f1,None, Some v_2)::t -> var_map = add_abs_to_node (Abs (v1,f1)) var_map  in
															 var_map
				|_::t -> helper2 var_map t

				in 
	let helper1 all_f var_map = 
			match all_f with 
			|[]-> helper2 var_map !info
			|f::t -> let(var_map,x) = (find_feat_link_opt v1_node var_map f [] ) in
							 let(var_map,y) = (find_feat_link_opt v2_node var_map f [] ) in
							 info := (f,x,y)::!info in
							 match !info with 
							 |(f1,Some v_1,Some v_2)::_ -> if(v_1<>v_2) then var_map
							 													else helper1 t var_map
							 |_::_ -> helper1 t var_map
			in
		helper1 fl var_map
									  

















(*EXAMPLE-TEST*)

let v1:var = 1
let v2:var = 2
let v3:var = 3
let v4:var = 4
let v5:var = 5
let v6:var = 6
let v7:var = 7
let v8:var = 8
let v9:var = 9
let v10:var = 10
let v11:var = 11
let v12:var = 12

let f1:feature = "lib"
let f2:feature = "share"
let f3:feature = "bin"
let f4:feature = "usr"
let f5:feature = "racid"
let f6:feature = "apache.conf"
let f7:feature = "lg.conf"
let f8:feature = "etc"


let (clau_1:clause) = [Feat(v1,f1,v2);Feat(v1,f2,v3);Feat(v4,f3,v6);Feat(v4,f4,v7);Eqf(v1,[f1;f2],v7);
					Feat(v8,f8,v9);Eqf(v8,[f4],v4);Feat(v9,f5,v10);Feat(v10,f6,v11);Feat(v10,f7,v12)]

(*  [Feat (1, "lib", 2); Feat (1, "share", 3); Feat (4, "bin", 6);
   Feat (4, "usr", 7); Eqf (1, ["lib"; "share"], 7); Feat (8, "etc", 9);
   Eqf (8, ["usr"], 4); Feat (9, "racid", 10); Feat (10, "apache.conf", 11);
   Feat (10, "lg.conf", 12)]
*)
let (var_map:var_map_type) = create_empty_var_map clau_1 VarMap.empty


let var_map1 = clause_feat_abs clau_1 var_map
let var_map2 = clause_eqf_eq clau_1 var_map1
let var_map3 = dissolve_all var_map2
let () = var_map_display var_map

(*
#trace dissolve_all;;
#trace disolve_node;;
#trace find_feat_link_opt;;
*)
