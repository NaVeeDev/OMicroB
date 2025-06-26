open Data 

let type_error expected got =
  failwith "TODO type_error"
(* Format.eprintf "Error: expected %s, got %a@." expected pp_print_value (Ptr.create got); assert false *)

let initial_env =
  Envir.empty_env
  (* |> declare_exn "Out_of_memory" *)
  (* |> declare_exn "Not_found" *)
  (* |> declare_exn "Exit" *)
  (* |> declare_exn "Invalid_argument"
  |> declare_exn "Failure"
  |> declare_exn "Match_failure"
  |> declare_exn "Stack_overflow"
  |> declare_exn "Assert_failure"
  |> declare_exn "Sys_blocked_io"
  |> declare_exn "Sys_error"
  |> declare_exn "End_of_file"
  |> declare_exn "Division_by_zero"
  |> declare_exn "Undefined_recursive_module"
  |> declare_builtin_constructor "false" 0
  |> declare_builtin_constructor "true" 1
  |> declare_builtin_constructor "None" 0
  |> declare_builtin_constructor "Some" 0
  |> declare_builtin_constructor "[]" 0
  |> declare_builtin_constructor "::" 0
  |> declare_builtin_constructor "()" 0 *)

(* let out_of_memory_exn = Runtime_lib.exn0 initial_env "Out_of_memory" *)

(* let not_found_exn = Runtime_lib.exn0 initial_env "Not_found" *)

(* let exit_exn = Runtime_lib.exn0 initial_env "Exit" *)

(* let invalid_argument_exn =
  Runtime_lib.exn1 initial_env "Invalid_argument" wrap_string

let failure_exn = Runtime_lib.exn1 initial_env "Failure" wrap_string *)

(* let match_failure_exn =
  Runtime_lib.exn3 initial_env "Match_failure" wrap_string wrap_int wrap_int *)

let stack_overflow_exn =
  Runtime_lib.exn0 initial_env "Stack_overflow"

(* let assert_failure_exn =
  Runtime_lib.exn3 initial_env "Assert_failure" wrap_string wrap_int wrap_int *)

(* let sys_blocked_io_exn = Runtime_lib.exn0 initial_env "Sys_blocked_io" *)

(* let sys_error_exn = Runtime_lib.exn1 initial_env "Sys_error" wrap_string *)

(* let end_of_file_exn = Runtime_lib.exn0 initial_env "End_of_file" *)

(* let division_by_zero_exn = Runtime_lib.exn0 initial_env "Division_by_zero" *)

(* let undefined_recursive_module_exn =
  Runtime_lib.exn3
    initial_env
    "Undefined_recursive_module"
    wrap_string
    wrap_int
    wrap_int *)

let wrap_exn = function
  (* | Out_of_memory -> Some out_of_memory_exn *)
  (* | Not_found -> Some not_found_exn *)
  (* | Exit -> Some exit_exn *)
  (* | Invalid_argument s -> Some (invalid_argument_exn s) *)
  (* | Failure s -> Some (failure_exn s) *)
  (* | Match_failure (s, i, j) -> Some (match_failure_exn s i j) *)
  | Stack_overflow -> Some stack_overflow_exn
  (* | Assert_failure (s, i, j) -> Some (assert_failure_exn s i j) *)
  (* | Sys_blocked_io -> Some sys_blocked_io_exn *)
  (* | Sys_error s -> Some (sys_error_exn s) *)
  (* | End_of_file -> Some end_of_file_exn *)
  (* | Division_by_zero -> Some division_by_zero_exn *)
  (* | Undefined_recursive_module (s, i, j) ->
    Some (undefined_recursive_module_exn s i j) *)
  | _ -> None
