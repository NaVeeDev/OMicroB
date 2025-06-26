open Data
open Envir
open Runtime_lib
open Runtime_base

let exp_of_desc loc desc =
  Parsetree.{
    pexp_desc = desc;
    pexp_loc = loc;
    pexp_loc_stack = [loc];
    pexp_attributes = [];
  }

let seq_or loc = function
  | [ (_, arg1); (_, arg2) ] ->
    let open Parsetree in
    let open Asttypes in
    let expr_true =
      Pexp_construct ({ txt = Longident.Lident "true"; loc }, None)
    in
    Some
      (exp_of_desc
         loc
         (Pexp_ifthenelse (arg1, exp_of_desc loc expr_true, Some arg2)))
  | _ -> None

let seq_and loc = function
  | [ (_, arg1); (_, arg2) ] ->
    let open Parsetree in
    let open Asttypes in
    let expr_false =
      Pexp_construct ({ txt = Longident.Lident "false"; loc }, None)
    in
    Some
      (exp_of_desc
         loc
         (Pexp_ifthenelse (arg1, arg2, Some (exp_of_desc loc expr_false))))
  | _ -> None

let apply loc = function
  | [ (_, f); (_, x) ] ->
    let open Parsetree in
    let open Asttypes in
    Some (exp_of_desc loc (Pexp_apply (f, [ (Nolabel, x) ])))
  | _ -> None

let rev_apply loc = function
  | [ (_, x); (_, f) ] ->
    let open Parsetree in
    let open Asttypes in
    Some (exp_of_desc loc (Pexp_apply (f, [ (Nolabel, x) ])))
  | _ -> None

external reraise : exn -> 'a = "%reraise"
external raise_notrace : exn -> 'a = "%raise_notrace"

module Prim = struct
  external spacetime_enabled : unit -> bool = "caml_spacetime_enabled"
  external time_include_children : bool -> float = "caml_sys_time_include_children"
  external isatty : out_channel -> bool = "caml_sys_isatty"
end

module Int32 = struct
  external neg : int32 -> int32 = "caml_int32_neg"
  external of_string : string -> int32 = "caml_int32_of_string"
  external to_int : int32 -> int = "caml_int32_to_int"
  external of_int : int -> int32 = "caml_int32_of_int"
end

let prims =
  [ ("%apply", ptr @@ Fexpr apply);
    ("%revapply", ptr @@ Fexpr rev_apply);
    ("%raise", ptr @@ Prim (fun v -> raise (InternalException v)));
    ("%reraise", ptr @@ Prim (fun v -> reraise (InternalException v)));
    ("%raise_notrace", ptr @@ Prim (fun v -> raise_notrace (InternalException v)));
    ("%sequand", ptr @@ Fexpr seq_and);
    ("%sequor", ptr @@ Fexpr seq_or);
    ("%identity", ptr @@ Prim (fun x -> x));
    ("caml_register_named_value",
     ptr @@ Prim (fun _ -> ptr @@ Prim (fun _ -> unit)));
    ("%makemutable",
     ptr @@ Prim (fun v -> ptr @@ Record (SMap.singleton "contents" (ref v))));
    ( "%field0",
      ptr @@ Prim
        (onptr @@ function
        | Record r -> !(SMap.find "contents" r)
        | Tuple l -> List.hd l
        | _ -> assert false) );
    ( "%field1",
      ptr @@ Prim
        (onptr @@ function
        | Tuple l -> List.hd (List.tl l)
        | _ -> assert false) );
    ( "%setfield0",
      ptr @@ Prim
        (onptr @@ function
        | Record r ->
          ptr @@ Prim
            (fun v ->
              SMap.find "contents" r := v;
              unit)
        | _ -> assert false) );
    (* ( "%incr",
      ptr @@ Prim
        (onptr @@ function
        | Record r ->
          let z = SMap.find "contents" r in
          z := wrap_int (unwrap_int !z + 1);
          unit
        | _ -> assert false) );
    ( "%decr",
      ptr @@ Prim
        (onptr @@ function
        | Record r ->
          let z = SMap.find "contents" r in
          z := wrap_int (unwrap_int !z - 1);
          unit
        | _ -> assert false) ); *)
    ("%ignore", ptr @@ Prim (fun _ -> unit));
    (* ("%big_endian", ptr @@ Prim (fun _ -> wrap_bool Sys.big_endian)); *)
    ("%word_size", ptr @@ Prim (fun _ -> ptr @@ Int 64));
    ("%int_size", ptr @@ Prim (fun _ -> ptr @@ Int 64));
    ("%max_wosize", ptr @@ Prim (fun _ -> ptr @@ Int 1000000));
    (* ("%ostype_unix", ptr @@ Prim (fun _ -> wrap_bool false));
    ("%ostype_win32", ptr @@ Prim (fun _ -> wrap_bool false));
    ("%ostype_cygwin", ptr @@ Prim (fun _ -> wrap_bool false)); *)
    (* ( "%backend_type", ptr @@
      Prim (fun _ ->
          ptr @@ Constructor ("Other", 0, Some (wrap_string "Interpreter")))
    ); *)
    ("%bytes_to_string", ptr @@ Prim (fun v -> v));
    ("%bytes_of_string", ptr @@ Prim (fun v -> v));
  ]

let prims =
  List.fold_left (fun env (name, v) -> SMap.add name v env) SMap.empty prims

let apply_ref =
  ref
    (fun _ _ -> assert false
      : value -> (Asttypes.arg_label * value) list -> value)

let () = apply_ref := Eval.apply prims
