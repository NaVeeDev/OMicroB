open Data 

val type_error : string -> Data.value_ -> 'a
val initial_env : env
(* val not_found_exn : value *)
(* val exit_exn : value *)
(* val invalid_argument_exn : string -> value *)
(* val failure_exn : string -> value *)
(* val match_failure_exn : string -> int -> int -> value
val assert_failure_exn : string -> int -> int -> value *)
(* val sys_blocked_io_exn : value *)
(* val sys_error_exn : string -> value *)
(* val end_of_file_exn : value *)
(* val division_by_zero_exn : value *)
(* val undefined_recursive_module_exn :
  string -> int -> int -> value *)
val wrap_exn : exn -> value option
