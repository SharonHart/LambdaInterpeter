(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


(*
  ADD FUNCTIONS BELOW
*)

let rec fv = function
	| Variable s			-> StringSet.singleton s
	| Abstraction (s, t) 	-> StringSet.remove s (fv t)
	| Application (t1, t2)	-> StringSet.union (fv t1) (fv t2)


let fresh_var used_vars : string = 
		if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
		then raise (OutOfVariablesError)
		else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)

		
		
let rec substitute (x : string) t1 t2 = match t2 with
			| Variable id when id = x 				-> t1
			| Variable id when id != x 				-> Variable id
			| Application(sub_t1, sub_t2)			-> Application ((substitute x t1 sub_t1), (substitute x t1 sub_t2))
			| Abstraction (id, sub_t) when id = x	-> Abstraction (id, sub_t)
			| Abstraction (id, sub_t) when (id != x && not(StringSet.mem id (fv t1))) -> Abstraction(id, substitute x t1 sub_t)
			| Abstraction (id, sub_t) when (id != x && StringSet.mem id (fv t1)) -> let new_var = fresh_var (StringSet.union (StringSet.union (fv t1) (fv sub_t)) (StringSet.singleton x)) in Abstraction(new_var	, substitute x t1 (substitute id (Variable new_var )  sub_t))
			| _ -> raise OutOfVariablesError (*to eliminate warnings.... not possible to get here*)
	
								
    


let reduce_strict = function
    | Application (t1, t2) -> (match t1 with 
                                | Abstraction (id1, t1') -> (match t2 with
                                                            | Abstraction (_, _) -> Some (substitute id1 t2 t1')
                                                            | _ -> None
                                )
                                | _ -> None
    )
    | _ -> None


let reduce_lazy t = 
    match t with
    | Application (t1, t2) -> let t1' = reduce_strict t1 in (
                                match t1' with
                                | Some t1'' -> Some (Application (t1'', t2))
                                | _ -> reduce_strict t
    )
    | _ -> None


let reduce_normal t =
    match t with
    | Abstraction (id, t') -> let t'' = reduce_strict t' in (
                                match t'' with
                                | Some t''' -> Some (Abstraction (id, t'''))
                                | _ -> None
    )
    | Application (t1, t2) -> let t' = reduce_strict t in (
                                match t' with
                                | Some _ -> t'
                                | _ -> let t1' = reduce_strict t1 in (
                                        match t1' with
                                        | Some t1'' -> Some (Application (t1'', t2))
                                        | _ -> let t2' = reduce_strict t2 in (
                                            match t2' with
                                            | Some t2'' -> Some (Application (t1, t2''))
                                            | _ -> None
                                        )
                                )
    )
    | _ -> None