(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file ~unsafe ~optimize ~entry_point ~invoke_with_symbols filename model
    =
  let next =
    let next = ref ~-1 in
    fun () ->
      incr next;
      !next
  in

  let module M :
    Wasm_ffi_intf.S0
      with type 'a t = 'a
       and type memory = Concrete_memory.t
       and module Value = Concrete_value = struct
    type 'a t = 'a

    type memory = Concrete_memory.t

    module Value = Concrete_value

    let assume _ = ()

    let assert' n =
      if not @@ Prelude.Int32.equal n 0l then begin
        Fmt.epr "Assertion failure was correctly reached\n";
        exit 0
      end

    let symbol_i32 () =
      match model.(next ()) with
      | V.I32 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a i32 value." V.pp v;
        assert false

    let symbol_char () =
      match model.(next ()) with
      | V.I32 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a char (i32) value." V.pp v;
        assert false

    let symbol_bool = symbol_char

    let abort () = assert false

    let alloc (_ : memory) (_ : Value.int32) (_ : Value.int32) = assert false

    let free (_ : memory) = assert false

    let exit (n : Value.int32) = exit (Int32.to_int n)

    let symbol_i8 () =
      match model.(next ()) with
      | V.I32 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a i8 (i32) value." V.pp v;
        assert false

    let symbol_i64 () =
      match model.(next ()) with
      | V.I64 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a i64 value." V.pp v;
        assert false

    let symbol_f32 () =
      match model.(next ()) with
      | V.F32 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a f32 value." V.pp v;
        assert false

    let symbol_f64 () =
      match model.(next ()) with
      | V.F64 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a f64 value." V.pp v;
        assert false

    let symbol_range _ _ =
      match model.(next ()) with
      | V.I32 n -> n
      | v ->
        Fmt.epr "Got value %a but expected a i32 value." V.pp v;
        assert false

    let print_char c = Fmt.pr "%c" (char_of_int (Int32.to_int c))

    let in_replay_mode () = 1l
  end in
  let replay_extern_module =
    let open M in
    let functions =
      [ ( "i8_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8)
        )
      ; ( "char_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_char)
        )
      ; ( "i32_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
        )
      ; ( "i64_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
        )
      ; ( "f32_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
        )
      ; ( "f64_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
        )
      ; ( "bool_symbol"
        , Concrete_extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_bool)
        )
      ; ( "range_symbol"
        , Concrete_extern_func.Extern_func
            (Func (Arg (I32, Arg (I32, Res)), R1 I32), symbol_range) )
      ; ( "assume"
        , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), assume)
        )
      ; ( "assert"
        , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), assert')
        )
      ; ( "in_replay_mode"
        , Concrete_extern_func.Extern_func
            (Func (UArg Res, R1 I32), in_replay_mode) )
      ; ( "print_char"
        , Concrete_extern_func.Extern_func
            (Func (Arg (I32, Res), R0), print_char) )
      ]
    in
    { Link.functions }
  in

  let link_state =
    Link.extern_module Link.empty_state ~name:"symbolic" replay_extern_module
  in

  let* m =
    Compile.File.until_binary_validate ~rac:false ~srac:false ~unsafe filename
  in
  let* m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let* m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let c = Interpret.Concrete.modul link_state.envs m in
  c

let cmd ~profiling ~debug ~unsafe ~optimize ~replay_file ~source_file
  ~entry_point ~invoke_with_symbols ~model_format =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let parse_fn =
    match model_format with
    | Cmd_utils.Scfg -> Smtml.Model.Parse.Scfg.from_file
    | Json -> Smtml.Model.Parse.Json.from_file
  in
  let* model =
    match parse_fn replay_file with
    | Error (`Msg msg) -> Error (`Invalid_model msg)
    | Ok model ->
      let bindings = Smtml.Model.get_bindings model in
      let+ model =
        list_map
          (fun (_sym, v) ->
            match v with
            | Smtml.Value.False -> Ok (V.I32 0l)
            | True -> Ok (V.I32 1l)
            | Num (I8 n) -> Ok (V.I32 (Int32.of_int n))
            | Num (I32 n) -> Ok (V.I32 n)
            | Num (I64 n) -> Ok (V.I64 n)
            | Num (F32 n) -> Ok (V.F32 (Float32.of_bits n))
            | Num (F64 n) -> Ok (V.F64 (Float64.of_bits n))
            | Unit | Int _ | Real _ | Str _ | List _ | App _ | Nothing ->
              Error
                (`Invalid_model
                   (Fmt.str "unexpected value type: %a" Smtml.Value.pp v) ) )
          bindings
      in
      Array.of_list model
  in
  let+ () =
    run_file ~unsafe ~optimize ~entry_point ~invoke_with_symbols source_file
      model
  in
  Fmt.pr "All OK@."
