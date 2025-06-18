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
end
module SSet : Set.S with type elt = string
type module_unit_id = Path of string
module UStore : Map.S with type key = module_unit_id

module Ptr :
  sig
    type 'a t
    val create : 'a -> 'a t
    exception Null
    val get : 'a t -> 'a
    val dummy : unit -> 'a t
    exception Full
    val backpatch : 'a t -> 'a -> unit
  end

val ptr : 'a -> 'a Ptr.t
val onptr : ('a -> 'b) -> 'a Ptr.t -> 'b

type value = value_ Ptr.t
and value_ =
    Int of int
  | Int32 of int32
  | Int64 of int64
  | Fun of Asttypes.arg_label * Parsetree.expression option *
      Parsetree.pattern * Parsetree.expression * env
  | Function of Parsetree.case list * env
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
  | Fun_with_extra_args of value * value list *
      (Asttypes.arg_label * value) SMap.t
  | Object of object_value

and fexpr =
    Location.t ->
    (Asttypes.arg_label * Parsetree.expression) list ->
    Parsetree.expression option

and 'a env_map = (bool * 'a) SMap.t

and env = {
  values : value_or_lvar env_map;
  modules : mdl env_map;
  constructors : int env_map;
  variant_types : string list env_map;
  classes : class_def env_map;
  current_object : object_value option;
}

and value_or_lvar =
    Value of value
  | Instance_variable of object_value * string

and class_def = Parsetree.class_expr * env ref

and mdl =
    Unit of module_unit_id * module_unit_state ref
  | Module of mdl_val
  | Functor of string * Parsetree.module_expr * env

and mdl_val = {
  mod_values : value SMap.t;
  mod_modules : mdl SMap.t;
  mod_constructors : int SMap.t;
  mod_variant_types : string list SMap.t;
  mod_classes : class_def SMap.t;
}

and module_unit_state = Not_initialized_yet | Initialized of mdl_val

and object_value = {
  env : env;
  self : Parsetree.pattern;
  initializers : expr_in_object list;
  named_parents : object_value SMap.t;
  variables : value ref SMap.t;
  methods : expr_in_object SMap.t;
  parent_view : string list;
}

and source_object = Current_object | Parent of object_value

and expr_in_object = {
  source : source_object;
  instance_variable_scope : SSet.t;
  named_parents_scope : SSet.t;
  expr : Parsetree.expression;
}

exception InternalException of value

val unit : value_ Ptr.t
val is_true : value_ Ptr.t -> bool
val pp_print_value : Format.formatter -> value_ Ptr.t -> unit
val pp_print_unit_id : Format.formatter -> module_unit_id -> unit
val read_caml_int : string -> int64
val value_of_constant : Parsetree.constant -> value_ Ptr.t
val value_compare : value_ Ptr.t -> value_ Ptr.t -> int
val value_equal : value_ Ptr.t -> value_ Ptr.t -> bool
val value_lt : value_ Ptr.t -> value_ Ptr.t -> bool
val value_le : value_ Ptr.t -> value_ Ptr.t -> bool
val value_gt : value_ Ptr.t -> value_ Ptr.t -> bool
val value_ge : value_ Ptr.t -> value_ Ptr.t -> bool
val next_exn_id : unit -> int
exception No_module_data
val get_module_data : Location.t -> mdl -> mdl_val
val module_name_of_unit_path : string -> string
val string_of_value : value -> string
val string_of_arg : value option -> string
val print_value_to_stdout : value -> unit
