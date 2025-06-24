open Asttypes
open Parsetree

(* module SMap = Map.Make (String) *)

module SMap : sig
  type key = string
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding_opt : 'a t -> (key * 'a) option
  val bindings : 'a t -> (key * 'a) list
  val cardinal : 'a t -> int
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) ->
              'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) ->
              'a t -> 'a t -> 'a t
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val add_to_list : key -> 'a -> 'a list t -> 'a list t
end = struct

  type key = string
  type 'a t = 
    | Empty
    | Node of 'a t * key * 'a * 'a t * int

  let height = function 
    | Empty -> 0
    | Node (_, _, _, _, h) -> h

  let create l k v r =
    let hl = height l in
    let hr = height r in
    Node (l, k, v, r, (max hl hr) + 1)
  
  let balance l k v r = 
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then begin
      match l with
      | Empty -> assert false
      | Node (ll, lk, lv, lr, _) -> 
        if height ll >= height lr then
          create ll lk lv (create lr k v r)
        else begin
          match lr with
          | Empty -> assert false
          | Node (lrl, lrk, lrv, lrr, _) ->
            create (create ll lk lv lrl) lrk lrv (create lrr k v r)
        end
    end
    else if hr > hl + 2 then begin
      match r with
      | Empty -> assert false
      | Node (rl, rk, rv, rr, _) ->
        if height rl <= height rr then
          create (create l k v rl) rk rv rr
        else begin
          match rl with
          | Empty -> assert false
          | Node (rll, rlk, rlv, rlr, _) ->
            create (create l k v rll) rlk rlv (create rlr rk rv rr)
        end
    end
    else create l k v r

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec add k v = function
    | Empty -> Node (Empty, k, v, Empty, 1)
    | Node (l, k', v', r, _) as m -> 
      let c = String.compare k k' in
      if c = 0 then Node (l, k, v, r, height m)
      else if c < 0 then balance (add k v l) k' v' r
      else balance l k' v' (add k v r)
  
  let singleton k v = Node (Empty, k, v, Empty, 1)

  let rec find k = function
    | Empty -> raise Not_found
    | Node (l, k', v, r, _) -> 
      let c = String.compare k k' in
      if c = 0 then v
      else if c < 0 then find k l
      else find k r
    
  let find_opt k m = try Some (find k m) with Not_found -> None

  let mem k m =
    try ignore (find k m); true
    with Not_found -> false

  let rec update k f = function
    | Empty -> (match f None with 
      | None -> Empty
      | Some v -> singleton k v)
    | Node (l, k', v, r, _) as m ->
      let c = String.compare k k' in
      if c = 0 then (match f (Some v) with
        | None -> simple_merge l r
        | Some v' -> Node (l, k, v', r, height m))
      else if c < 0 then balance (update k f l) k' v r
      else balance l k' v (update k f r) 

  and simple_merge l r =
    match l,r with
    | Empty, x | x, Empty -> x
    | _ -> let k,v = min_binding r in
            balance l k v (remove k r)

  and min_binding = function 
    | Empty -> raise Not_found
    | Node (Empty, k, v, _, _) -> (k, v)
    | Node (l, k, v, r, _) -> min_binding l
  
  and remove k = function
    | Empty -> Empty
    | Node (l, k', v, r, _) ->
      let c = String.compare k k' in
      if c = 0 then simple_merge l r
      else if c < 0 then balance (remove k l) k' v r
      else balance l k' v (remove k r)

  let rec min_binding_opt m = try Some (min_binding m) with Not_found -> None

  let rec max_binding = function
    | Empty -> raise Not_found
    | Node (_, k, v, Empty, _) -> (k, v)
    | Node (_, k, v, r, _) -> max_binding r

  let max_binding_opt m = try Some (max_binding m) with Not_found -> None

  let rec bindings = function
    | Empty -> []
    | Node (l, k, v, r, _) ->
      bindings l @ [(k, v)] @ bindings r
  
  let rec cardinal = function 
    | Empty -> 0
    | Node (l, _, _, r, _) -> cardinal l + 1 + cardinal r
      
  let choose m = min_binding m

  let choose_opt = min_binding_opt

  let rec iter f = function
    | Empty -> ()
    | Node (l, k, v, r, _) ->
      iter f l;
      f k v;
      iter f r

  let rec fold f m acc =
    match m with
    | Empty -> acc
    | Node (l, k, v, r, _) ->
      fold f l (f k v (fold f r acc))
  
  let rec map f = function
    | Empty -> Empty
    | Node (l, k, v, r, _) -> balance (map f l) k (f v) (map f r)

  let rec mapi f = function
    | Empty -> Empty
    | Node (l, k, v, r, _) ->
      balance (mapi f l) k (f k v) (mapi f r)

  let rec filter f = function 
    | Empty -> Empty
    | Node (l, k, v, r, _) ->
      let l' = filter f l in
      let r' = filter f r in
      if f k v then balance l' k v r'
      else simple_merge l' r'

  let rec filter_map f = function
    | Empty -> Empty 
    | Node (l, k, v, r, _) ->
      let l' = filter_map f l in
      let r' = filter_map f r in
      match f k v with
        | None -> simple_merge l' r'
        | Some v' -> balance l' k v' r'

  let rec partition f = function
    | Empty -> (Empty, Empty)
    | Node (l, k, v, r, _) ->
      let l1, l2 = partition f l in
      let r1, r2 = partition f r in
      if f k v then
        (balance l1 k v r1, simple_merge l2 r2)
      else
        (simple_merge l1 r1, balance l2 k v r2)

    let rec split k = function
      | Empty -> (Empty, None, Empty)
      | Node (l, k', v, r, _)->
        let c = String.compare k k' in
        if c = 0 then (l, Some v, r)
        else if c < 0 then
          let l1,pres, l2 = split k l in
          (l1, pres, balance l2 k' v r)
        else
          let r1,pres, r2 = split k r in
          (balance l k' v r1, pres, r2)

    let rec merge f m1 m2 =
      match m1, m2 with
        | Empty, Empty -> Empty
        | Empty, _ ->
            fold (fun k v acc ->
              match f k None (Some v) with
              | None -> acc
              | Some v' -> add k v' acc
            ) m2 Empty
        | _, Empty ->
            fold (fun k v acc ->
              match f k (Some v) None with
              | None -> acc
              | Some v' -> add k v' acc
            ) m1 Empty
        | Node (l1,k1,v1,r1,_ ), _ ->
            let l2, o2, r2 = split k1 m2 in
            let vo = f k1 (Some v1) o2 in
            let l = merge f l1 l2 in
            let r = merge f r1 r2 in
            match vo with
            | None -> simple_merge l r
            | Some v -> balance l k1 v r

    let union f m1 m2 = 
      merge (fun k v1 v2 ->
      match v1, v2 with
      | Some v1, Some v2 -> f k v1 v2
      | Some v, None | None, Some v -> Some v
      | None, None -> None
      ) m1 m2

    let rec equal cmp m1 m2 = 
      match m1,m2 with
      | Empty, Empty -> true
      | Node (l1, k1, v1, r1, _), Node (l2, k2, v2, r2, _) ->
        String.equal k1 k2 && cmp v1 v2 && equal cmp l1 l2 && equal cmp r1 r2
      | _ -> false
    
    let rec compare cmp m1 m2 =
      match m1, m2 with
        | Empty, Empty -> 0
        | Empty, _ -> -1
        | _, Empty -> 1
        | Node (l1, k1, v1, r1, _), Node (l2, k2, v2, r2, _) ->
          let c = String.compare k1 k2 in
          if c <> 0 then c
          else
            let c = cmp v1 v2 in
            if c <> 0 then c
            else
              let c = compare cmp l1 l2 in
              if c <> 0 then c else compare cmp r1 r2
      
    let rec for_all f = function
      | Empty -> true
      | Node (l, k, v, r, _) ->
        f k v && for_all f l && for_all f r
    
    let rec exists f = function
      | Empty -> false
      | Node (l, k, v, r, _) ->
        f k v || exists f l || exists f r

    let to_list m = bindings m
    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
    
    let rec find_first f = function
      | Empty -> raise Not_found
      | Node (l, k, v, r, _) ->
        if f k then 
          match find_first f l with
          | exception Not_found -> (k,v)
          |  res ->  res 
        else find_first f r
    
    let rec find_last f = function
      | Empty -> raise Not_found
      | Node (l, k, v, r, _) ->
        if f k then 
          match find_last f r with
          | exception Not_found -> (k,v)
          | res -> res
        else find_last f l

    let rec find_first_opt f m= try Some (find_first f m) with Not_found -> None
    let rec find_last_opt f m = try Some (find_last f m) with Not_found -> None

    let rec add_to_list k v = function
      | Empty -> singleton k [v]
      | Node (l, k', v', r, _) as m  ->
        let c = String.compare k k' in
        if c = 0 then Node (l, k, v::v', r, height m)
        else if c < 0 then balance (add_to_list k v l) k' v' r
        else balance l k' v' (add_to_list k v r)

end

module SSet = Set.Make (String)

type module_unit_id = Path of string
module UStore = Map.Make(struct
  type t = module_unit_id
  let compare (Path a) (Path b) = String.compare a b
end)

module Ptr : sig
  type 'a t
  val create : 'a -> 'a t

  exception Null
  val get : 'a t -> 'a

  val dummy : unit -> 'a t

  exception Full
  val backpatch : 'a t -> 'a -> unit
end = struct
  type 'a t = 'a option ref

  let create v = ref (Some v)

  exception Null
  let get ptr = match !ptr with
    | None -> raise Null
    | Some v -> v

  let dummy () = ref None

  exception Full
  let backpatch ptr v = match !ptr with
      | Some _ -> raise Full
      | None -> ptr := Some v
end

let ptr v = Ptr.create v
let onptr f = fun v -> f (Ptr.get v)

type value = value_ Ptr.t
and value_ =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Fun of arg_label * expression option * pattern * expression * env
  | Function of case list * env
  | String of bytes
  | Float of float
  | Tuple of value list
  | Constructor of string * int * value option
  | Poly_variant of string * value option
  | Prim of (value -> value)
  | Fexpr of fexpr
  | ModVal of mdl
  | InChannel of in_channel
  | OutChannel of out_channel
  | Record of value ref SMap.t
  | Lz of (unit -> value) ref
  | Array of value array
  | Fun_with_extra_args of value * value list * (arg_label * value) SMap.t
  | Object of object_value

and fexpr = Location.t -> (arg_label * expression) list -> expression option

and 'a env_map = (bool * 'a) SMap.t
(* the boolean tracks whether the value should be exported in the
   output environment *)

and env = {
  values : value_or_lvar env_map;
  modules : mdl env_map;
  constructors : int env_map;
  variant_types : string list env_map;
  classes : class_def env_map;
  current_object : object_value option;
}

and value_or_lvar =
  | Value of value
  | Instance_variable of object_value * string

and class_def = class_expr * env ref

and mdl =
  | Unit of module_unit_id * module_unit_state ref
  | Module of mdl_val
  | Functor of string * module_expr * env

and mdl_val = {
    mod_values : value SMap.t;
    mod_modules : mdl SMap.t;
    mod_constructors : int SMap.t;
    mod_variant_types : string list SMap.t;
    mod_classes : class_def SMap.t;
  }

and module_unit_state =
  | Not_initialized_yet
  | Initialized of mdl_val
(* OCaml calls a "compilation unit" the language object corresponding
   to a group of files of the same name with different extensions
   (foo.ml, foo.mli in source form, foo.cm* in compiled form). From
   the language those are also visible implicitly as modules (Foo),
   but they are not exactly identical to modules as well -- in what
   sort of dependencies are allowed between units, in particular.

   Instead of "compilation units" which sounds strange in an
   interpreter, we just call these "module units" or "units".

   In our value representation, some of the modules in the environment
   may in fact be units (a unit is a category of module), and
   initialized units contain module data, like a normal module.

   The -no-alias-deps flag allows an OCaml source fragment to create
   an alias to a unit that has not been evaluated yet -- allowing
   cyclic dependencies where each cycle contains a "weak" edge that is
   just a module-alias occurrence

   From an operational point of view, this corresponds to allowing
   implicit recursive definition of units, where all units can
   alias/reference each other (even units evaluated later), but a unit
   may only dereference (access the module data of) a unit evaluated
   earlier.

   To support these recursive definitions, we give backpatching
   semantics to unit definitions: all the units that are to be
   evaluated are loaded at once in the environment as (references to)
   "non initialized" units, and after each unit is evaluated to some
   module data we mutate its state in the environment, which makes
   non-aliasing uses possible for further units.
*)

and object_value = {
  env: env;
  self: pattern;
  initializers: expr_in_object list;
  named_parents: object_value SMap.t;
  variables: value ref SMap.t;
  methods: expr_in_object SMap.t;

  parent_view: string list;
  (* When evaluating a call super#foo, it would be wrong to just
     resolve the call in a parent object bound to the 'super'
     identifier in the current environment. Indeed, when then
     executing the code of super#foo, self-calls of the form self#bar
     would be resolved in the parent object 'super', instead of in the
     current object 'self', which is the intended late-binding
     semantics.

     To solve this issue, we bind 'super' not to the parent object,
     but to the *current* object "viewed as its parent 'super'"; the
     'parent_view' field stores that view (in general there may be
     several levels of super-calls nesting, so it's a list). The view
     affects how methods are resolved -- their code is looked up
     in the right parent object.
   *)
}
and source_object =
  | Current_object
  | Parent of object_value
and expr_in_object = {
  source : source_object;

  instance_variable_scope : SSet.t;
  (** The scoping of instance variables within object declarations
      makes them in the scope of only some of the expressions in methods
      and initializers; an "instance variable scope" remembers the set
      of instance variables that are in scope of a given expression). *)

  named_parents_scope : SSet.t;
  (** Similarly, it may be that only some of the 'inherit foo as x' fields
      scope over the current piece of code, so we keep a set of visible parents.

      Remark: the self-pattern is always at the beginning of a class
      or object declaration, so it is in the scope of all
      expressions. *)

  expr : expression;
}

exception InternalException of value

let unit = ptr @@ Constructor ("()", 0, None)

let is_true = onptr @@ function
  | Constructor ("true", _, None) -> true
  | Constructor ("false", _, None) -> false
  | _ -> assert false

let rec pp_print_value ff =
  failwith "TODO"
 (* onptr @@ function *)
  (* | Int n -> Format.fprintf ff "%d" n *)
  (* | Int32 n -> Format.fprintf ff "%ldl" n *)
  (* | Int64 n -> Format.fprintf ff "%LdL" n *)
  (* | Nativeint n -> Format.fprintf ff "%ndn" n *)
  (* | Fexpr _ -> Format.fprintf ff "<fexpr>" *)
  (* | Fun _ | Function _ | Prim _ | Lz _ | Fun_with_extra_args _ -> *)
  (*   Format.fprintf ff "<function>" *)
  (* | String s -> Format.fprintf ff "%S" (Bytes.to_string s) *)
  (* | Float f -> Format.fprintf ff "%f" f *)
  (* | Tuple l -> *)
  (*   Format.fprintf *)
  (*     ff *)
  (*     "(%a)" *)
  (*     (Format.pp_print_list *)
  (*        ~pp_sep:(fun ff () -> Format.fprintf ff ", ") *)
  (*        pp_print_value) *)
  (*     l *)
  (* | Constructor (c, d, arg) -> *)
  (*   Format.fprintf ff "%s#%d%a" c d pp_print_arg arg *)
  (* | Poly_variant (c, arg) -> *)
  (*   Format.fprintf ff "`%s%a" c pp_print_arg arg *)
  (* | ModVal _ -> Format.fprintf ff "<module>" *)
  (* | InChannel _ -> Format.fprintf ff "<in_channel>" *)
  (* | OutChannel _ -> Format.fprintf ff "<out_channel>" *)
  (* | Record r -> *)
  (*   Format.fprintf ff "{"; *)
  (*   SMap.iter (fun k v -> Format.fprintf ff "%s = %a; " k pp_print_value !v) r; *)
  (*   Format.fprintf ff "}" *)
  (* | Array a -> *)
  (*   Format.fprintf *)
  (*     ff *)
  (*     "[|%a|]" *)
  (*     (Format.pp_print_list *)
  (*        ~pp_sep:(fun ff () -> Format.fprintf ff "; ") *)
  (*        pp_print_value) *)
  (*     (Array.to_list a) *)
  (* | Object _ -> Format.fprintf ff "<object>" *)

and pp_print_arg ff = function
  | None -> ()
  | Some v -> print_string " "; pp_print_value ff v

let pp_print_unit_id ppf (Path s) =
  failwith "TODO"
  (* Format.fprintf ppf "%S" s *)

let read_caml_int s =
  let c = ref 0L in
  let sign, init =
    if String.length s > 0 && s.[0] = '-' then (Int64.of_int (-1), 1) else (1L, 0)
  in
  let base, init =
    if String.length s >= init + 2 && s.[init] = '0'
    then
      ( (match s.[init + 1] with
        | 'x' | 'X' -> 16L
        | 'b' | 'B' -> 2L
        | 'o' | 'O' -> 8L
        | _ -> assert false),
        init + 2 )
    else (10L, init)
  in
  for i = init to String.length s - 1 do
    match s.[i] with
    | x when '0' <= x && x <= '9' ->
      c := Int64.(add (mul base !c) (of_int (int_of_char x - int_of_char '0')))
    | x when 'a' <= x && x <= 'f' ->
      c :=
        Int64.(
          add (mul base !c) (of_int (int_of_char x - int_of_char 'a' + 10)))
    | x when 'A' <= x && x <= 'F' ->
      c :=
        Int64.(
          add (mul base !c) (of_int (int_of_char x - int_of_char 'A' + 10)))
    | '_' -> ()
    | _ ->
      (* Format.eprintf "FIXME literal: %s@." s; *)
      assert false
  done;
  Int64.mul sign !c

let value_of_constant const = ptr @@ match const with
  | Pconst_integer (s, None) -> Int (Int64.to_int (read_caml_int s))
  | Pconst_integer (s, Some 'l') -> Int32 (Int64.to_int32 (read_caml_int s))
  | Pconst_integer (s, Some 'L') -> Int64 (read_caml_int s)
  | Pconst_integer (_s, Some c) ->
    (* Format.eprintf "Unsupported suffix %c@." c; *)
    assert false
  | Pconst_char c -> Int (int_of_char c)
  | Pconst_float (f, _) -> Float (float_of_string f)
  | Pconst_string (s, _) -> String (Bytes.of_string s)

let rec value_compare v1 v2 = match Ptr.get v1, Ptr.get v2 with
  | Fun _, _
  | Function _, _
  | _, Fun _
  | _, Function _
  | Lz _, _
  | _, Lz _
  | Fun_with_extra_args _, _
  | _, Fun_with_extra_args _ ->
    failwith "tried to compare function"
  | ModVal _, _ | _, ModVal _ -> failwith "tried to compare module"
  | InChannel _, _ | OutChannel _, _ | _, InChannel _ | _, OutChannel _ ->
    failwith "tried to compare channel"
  | Fexpr _, _ | _, Fexpr _ -> failwith "tried to compare fexpr"
  | Prim _, _ | _, Prim _ -> failwith "tried to compare prim"
  | Object _, _ | _, Object _ -> failwith "tried to compare object"

  | Int n1, Int n2 -> compare n1 n2
  | Int _, _ -> assert false

  | Int32 n1, Int32 n2 -> compare n1 n2
  | Int32 _, _ -> assert false

  | Int64 n1, Int64 n2 -> compare n1 n2
  | Int64 _, _ -> assert false


  | Float f1, Float f2 -> compare f1 f2
  | Float _, _ -> assert false

  | String s1, String s2 -> compare s1 s2
  | String _, _ -> assert false

  | Constructor (c1, d1, arg1), Constructor (c2, d2, arg2) ->
    let c = compare (d1, c1) (d2, c2) in
    if c <> 0 then c else
    value_compare_arg arg1 arg2
  | Constructor _, _ -> assert false

  | Poly_variant (c1, arg1), Poly_variant (c2, arg2) ->
    let c = compare c1 c2 in
    if c <> 0 then c else
    value_compare_arg arg1 arg2
  | Poly_variant _, _ -> assert false

  | Tuple l1, Tuple l2 ->
    assert (List.length l1 = List.length l2);
    List.fold_left2
      (fun cur x y -> if cur = 0 then value_compare x y else cur)
      0
      l1
      l2
  | Tuple _, _ -> assert false

  | Record r1, Record r2 ->
    let map1 =
      SMap.merge
        (fun _ u v ->
          match (u, v) with
          | None, None -> None
          | None, Some _ | Some _, None -> assert false
          | Some u, Some v -> Some (!u, !v))
        r1
        r2
    in
    SMap.fold
      (fun _ (u, v) cur -> if cur = 0 then value_compare u v else cur)
      map1
      0
  | Record _, _ -> assert false

  | Array a1, Array a2 ->
    let comp_len = compare (Array.length a1) (Array.length a2) in
    if comp_len <> 0 then comp_len
    else (
      let cmp = ref 0 in
      let count = ref 0 in
      while !cmp = 0 && !count < Array.length a1 do
        cmp := value_compare a1.(!count) a2.(!count);
        incr count
      done;
      !cmp
    )
  | Array _, _ -> assert false

and value_compare_arg arg1 arg2 =
  match arg1, arg2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some v1, Some v2 -> value_compare v1 v2

let value_equal v1 v2 = value_compare v1 v2 = 0

let value_lt v1 v2 = value_compare v1 v2 < 0
let value_le v1 v2 = value_compare v1 v2 <= 0
let value_gt v1 v2 = value_compare v1 v2 > 0
let value_ge v1 v2 = value_compare v1 v2 >= 0

let next_exn_id =
  let last_exn_id = ref (-1) in
  fun () ->
    incr last_exn_id;
    !last_exn_id

exception No_module_data
let get_module_data loc = function
  | Module data -> data
  | Functor _ ->
     (* Format.eprintf "%a@.Tried to access the components of a functor@." *)
     (*   Location.print_loc loc; *)
     raise No_module_data
  | Unit (unit_id, unit_state) ->
     begin match !unit_state with
       | Initialized data -> data
       | exception Not_found ->
          (* Format.eprintf "%a@.Tried to access the undeclared unit %a@." *)
          (*  Location.print_loc loc *)
          (*  pp_print_unit_id unit_id; *)
          raise No_module_data
       | Not_initialized_yet ->
          (* Format.eprintf "%a@.unit %a is not yet initialized@." *)
          (*   Location.print_loc loc *)
          (*   pp_print_unit_id unit_id; *)
          raise No_module_data
     end

let module_name_of_unit_path path =
  if path = "ocaml.py" then
    "Ocaml"
  else begin

    (* print_string "path = ";
    print_endline path; *)

    (* This function is used to convert a unit path (e.g. "foo/bar/baz.ml") *)
    (* into a module name (e.g. "Foo_bar_baz"). It is used to create the *)
    (* module name for the unit when it is loaded into the environment. *)
    (* The module name is derived from the path by capitalizing each part *)
    (* of the path and joining them with underscores. *)
    (* The path is expected to be a valid unit path, i.e. it should not contain *)
    (* any invalid characters or be empty. *)
    (* The function currently raises an exception, as it is not yet implemented. *)
    (* failwith "TODO module_name_of_unit_path" *)
    let n = String.length path in
    let guessed_ml_extension = String.sub path (n - 3) 3 in
    let path_without_extension =
      if guessed_ml_extension = ".ml" then
        String.sub path 3 (n - 6)
      else
        String.sub path 3 (n - 3)
    in

    (* print_string "=> path_without_extension = ";
    print_endline path_without_extension; *)

    (* We remove the ".ml" extension from the path, as it is not needed for the module name. *)
    (* The module name is derived from the path by capitalizing each part and joining them with underscores. *)
    (* The path is expected to be a valid unit path, i.e. it should not contain any invalid characters or be empty. *)
    (* The function currently raises an exception, as it is not yet implemented. *)
    (* failwith "TODO module_name_of_unit_path" *)
    let module_name = String.split_on_char '/' path_without_extension
      |> List.map String.capitalize_ascii
      |> String.concat "_"
      |> String.capitalize_ascii
      |> String.map (function ' ' -> '_' | c -> c)
      |> String.trim
    in

    (* print_string "==> module_name = ";
    print_endline module_name; *)

    module_name
  end
  (* XXX: this was the previous implementation, but the Filename module is not available, so we hack it away (see above). *)
  (* path *)
  (* |> Filename.basename *)
  (* |> Filename.remove_extension *)
  (* |> String.capitalize_ascii *)

  let rec string_of_value (arg : value) : string =
    match (Ptr.get arg) with
    | Int n -> string_of_int n
    | Int32 n -> Int32.to_string n ^ "l" (* Standard way to represent int32 literals *)
    | Int64 n -> Int64.to_string n ^ "L" (* Standard way to represent int64 literals *)
    (* | Nativeint n -> Nativeint.to_string n ^ "n" (* Standard way to represent nativeint literals *) *)
    | Fexpr _ -> "<fexpr>"
    | Fun _ | Function _ | Prim _ | Lz _ | Fun_with_extra_args _ ->
      "<function>"
    | String s -> "\"" ^ (Bytes.to_string (Bytes.escaped s)) ^ "\""
    | Float f -> string_of_float f
    | Tuple l ->
      "(" ^ (String.concat ", " (List.map string_of_value l)) ^ ")"
    | Constructor (c, d, arg) ->
      c ^ "#" ^ (string_of_int d) ^ (string_of_arg arg)
    | Poly_variant (c, arg) ->
      "`" ^ c ^ (string_of_arg arg)
    | ModVal _ -> "<module>"
    | InChannel _ -> "<in_channel>"
    | OutChannel _ -> "<out_channel>"
    | Record r ->
      let fields =
        SMap.fold (fun k v acc ->
          (k ^ " = " ^ (string_of_value !v)) :: acc
        ) r []
      in
      "{ " ^ (String.concat "; " (List.rev fields)) ^ " }" (* List.rev to maintain insertion order if any *)
    | Array a ->
      "[|" ^ (String.concat "; " (List.map string_of_value (Array.to_list a))) ^ "|]"
    | Object _ -> "<object>"

and string_of_arg (arg : value option) : string =
  match arg with
  | None -> ""
  | Some v -> " " ^ (string_of_value v)

let print_value_to_stdout (v : value) : unit =
  print_string (string_of_value v);
  print_newline ()