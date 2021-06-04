type feature = string

let compare = compare
let equal f1 f2 = compare f1 f2 = 0

module Feat = struct
  type t = feature

  let compare = compare
end

module FSet = Set.Make(Feat)
module FMap = Map.Make(Feat)


(*
let myset = FSet.add "abc" FSet.empty

FSet.elements myset;;
*)


type var = int
type node = {feat: (feature*node) list;
       abFeat: feature list}

let compare = compare
let equal m n = compare m n = 0

module Var = struct
  type t = var

  let compare = compare
end

module VarMap = Map.Make(Var)

type var_map = node VarMap.t 
(*Keys are var and values are node*)
(*let m1 = VarMap.add 1 empty_node VarMap.empty
 VarMap.find 1 m1;;
  let m1 = VarMap.add 2 empty_node m1*)

let empty_node ():node = {feat= FMap.empty;abFeat=FSet.empty}

type atom =
  | Eq of var * var
  | Feat of var * feature * var
  | Abs of var * var
  | Fen of var * (feature list)
  | Sim of var * (feature list) * var

type clause = atom list


let v1:var = 1;
let v2:var = 2;
let v3:var = 3;
let v4:var = 4;

let f1:feature = "a";
let f2:feature = "b";
let f3:feature = "c";
let f4:feature = "d";

let clau_1:clause = [Feat(v1,f1,v2),Feat(v1,f2,v4),Feat(v1,f3,v3)]