(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Smtml
open Ty
open Expr

type vbool = Expr.t

type int32 = Expr.t

let pp_int32 = Expr.pp

type int64 = Expr.t

let pp_int64 = Expr.pp

type float32 = Expr.t

let pp_float32 = Expr.pp

type float64 = Expr.t

let pp_float64 = Expr.pp

type externref = Concrete_value.externref

type ref_value =
  | Funcref of Func_intf.t option
  | Externref of externref option

let pp_ref_value _fmt _v = assert false

type t =
  | I32 of int32
  | I64 of int64
  | F32 of float32
  | F64 of float64
  | Ref of ref_value

let const_i32 (i : Int32.t) : int32 = value (Num (I32 i))

let const_i64 (i : Int64.t) : int64 = value (Num (I64 i))

let const_f32 (f : Float32.t) : float32 = value (Num (F32 (Float32.to_bits f)))

let const_f64 (f : Float64.t) : float64 = value (Num (F64 (Float64.to_bits f)))

let ref_null _ty = Ref (Funcref None)

let ref_func f : t = Ref (Funcref (Some f))

let ref_externref t v : t = Ref (Externref (Some (E (t, v))))

let ref_is_null = function
  | Funcref (Some _) | Externref (Some _) -> value False
  | Funcref None | Externref None -> value True

let pp ppf v =
  let e =
    match v with
    | I32 e -> e
    | I64 e -> e
    | F32 e -> e
    | F64 e -> e
    | Ref _ -> assert false
  in
  Expr.pp ppf e

module Ref = struct
  let get_func (r : ref_value) : Func_intf.t Value_intf.get_ref =
    match r with
    | Funcref (Some f) -> Ref_value f
    | Funcref None -> Null
    | Externref _ -> Type_mismatch

  let get_externref (type t) (r : ref_value) (t : t Type.Id.t) :
    t Value_intf.get_ref =
    match r with
    | Externref (Some (E (ety, v))) -> (
      match Type.Id.provably_equal t ety with
      | None -> assert false
      | Some Equal -> Ref_value v )
    | _ -> assert false
end

module Bool = struct
  let const b = Bool.v b

  let not e = Bool.not e

  let or_ e1 e2 = Bool.or_ e1 e2

  let and_ e1 e2 = Bool.and_ e1 e2

  let int32 e =
    match view e with
    | Val True -> const_i32 1l
    | Val False -> const_i32 0l
    | Cvtop (Ty_bitv 32, ToBool, e') -> e'
    | _ -> make (Cvtop (Ty_bitv 32, OfBool, e))

  let select_expr c ~if_true ~if_false = Bool.ite c if_true if_false

  let pp ppf (e : vbool) = Expr.pp ppf e
end

module I32 = struct
  let ty = Ty_bitv 32

  let zero = const_i32 0l

  let clz e = unop ty Clz e

  let ctz e = unop ty Ctz e

  let popcnt _ =
    (* TODO *)
    assert false

  let add e1 e2 = binop ty Add e1 e2

  let sub e1 e2 = binop ty Sub e1 e2

  let mul e1 e2 = binop ty Mul e1 e2

  let div e1 e2 = binop ty Div e1 e2

  let unsigned_div e1 e2 = binop ty DivU e1 e2

  let rem e1 e2 = binop ty Rem e1 e2

  let unsigned_rem e1 e2 = binop ty RemU e1 e2

  let boolify e =
    match view e with
    | Val (Num (I32 0l)) -> Some (Bool.const false)
    | Val (Num (I32 1l)) -> Some (Bool.const true)
    | Cvtop (_, OfBool, cond) -> Some cond
    | _ -> None

  let logand e1 e2 =
    match (boolify e1, boolify e2) with
    | Some b1, Some b2 -> Bool.int32 (Bool.and_ b1 b2)
    | _ -> binop ty And e1 e2

  let logor e1 e2 =
    match (boolify e1, boolify e2) with
    | Some b1, Some b2 -> Bool.int32 (Bool.or_ b1 b2)
    | _ -> binop ty Or e1 e2

  let logxor e1 e2 = binop ty Xor e1 e2

  let shl e1 e2 = binop ty Shl e1 e2

  let shr_s e1 e2 = binop ty ShrA e1 e2

  let shr_u e1 e2 = binop ty ShrL e1 e2

  let rotl e1 e2 = binop ty Rotl e1 e2

  let rotr e1 e2 = binop ty Rotr e1 e2

  let eq_const e c =
    match view e with
    | Cvtop (_, OfBool, cond) -> begin
      match c with 0l -> Bool.not cond | 1l -> cond | _ -> Bool.const false
    end
    | _ -> relop Ty_bool Eq e (const_i32 c)

  let eq e1 e2 =
    if phys_equal e1 e2 then Bool.const true else relop Ty_bool Eq e1 e2

  let ne e1 e2 =
    if phys_equal e1 e2 then Bool.const false else relop Ty_bool Ne e1 e2

  let lt e1 e2 =
    if phys_equal e1 e2 then Bool.const false else relop ty Lt e1 e2

  let gt e1 e2 =
    if phys_equal e1 e2 then Bool.const false else relop ty Gt e1 e2

  let lt_u e1 e2 =
    if phys_equal e1 e2 then Bool.const false else relop ty LtU e1 e2

  let gt_u e1 e2 =
    if phys_equal e1 e2 then Bool.const false else relop ty GtU e1 e2

  let le e1 e2 = if phys_equal e1 e2 then Bool.const true else relop ty Le e1 e2

  let ge e1 e2 = if phys_equal e1 e2 then Bool.const true else relop ty Ge e1 e2

  let le_u e1 e2 =
    if phys_equal e1 e2 then Bool.const true else relop ty LeU e1 e2

  let ge_u e1 e2 =
    if phys_equal e1 e2 then Bool.const true else relop ty GeU e1 e2

  let to_bool (e : vbool) =
    match view e with
    | Val (Num (I32 i)) -> Bool.const @@ Int32.ne i 0l
    | Cvtop (_, OfBool, cond) -> cond
    | _ -> make (Cvtop (ty, ToBool, e))

  let trunc_f32_s x = cvtop ty TruncSF32 x

  let trunc_f32_u x = cvtop ty TruncUF32 x

  let trunc_f64_s x = cvtop ty TruncSF64 x

  let trunc_f64_u x = cvtop ty TruncUF64 x

  let trunc_sat_f32_s _ = assert false

  let trunc_sat_f32_u _ = assert false

  let trunc_sat_f64_s _ = assert false

  let trunc_sat_f64_u _ = assert false

  let reinterpret_f32 x = cvtop ty Reinterpret_float x

  let wrap_i64 x = cvtop ty WrapI64 x

  (* FIXME: This is probably wrong? *)
  let extend_s n x =
    cvtop ty (Sign_extend (32 - n)) (make (Extract (x, n / 8, 0)))
end

module I64 = struct
  let ty = Ty_bitv 64

  let zero = const_i64 0L

  let clz e = unop ty Clz e

  let ctz e = unop ty Ctz e

  let popcnt _ =
    (* TODO *)
    assert false

  let add e1 e2 = binop ty Add e1 e2

  let sub e1 e2 = binop ty Sub e1 e2

  let mul e1 e2 = binop ty Mul e1 e2

  let div e1 e2 = binop ty Div e1 e2

  let unsigned_div e1 e2 = binop ty DivU e1 e2

  let rem e1 e2 = binop ty Rem e1 e2

  let unsigned_rem e1 e2 = binop ty RemU e1 e2

  let logand e1 e2 = binop ty And e1 e2

  let logor e1 e2 = binop ty Or e1 e2

  let logxor e1 e2 = binop ty Xor e1 e2

  let shl e1 e2 = binop ty Shl e1 e2

  let shr_s e1 e2 = binop ty ShrA e1 e2

  let shr_u e1 e2 = binop ty ShrL e1 e2

  let rotl e1 e2 = binop ty Rotl e1 e2

  let rotr e1 e2 = binop ty Rotr e1 e2

  let eq_const e c = relop Ty_bool Eq e (const_i64 c)

  let eq e1 e2 = relop Ty_bool Eq e1 e2

  let ne e1 e2 = relop Ty_bool Ne e1 e2

  let lt e1 e2 = relop ty Lt e1 e2

  let gt e1 e2 = relop ty Gt e1 e2

  let lt_u e1 e2 = relop ty LtU e1 e2

  let gt_u e1 e2 = relop ty GtU e1 e2

  let le e1 e2 = relop ty Le e1 e2

  let ge e1 e2 = relop ty Ge e1 e2

  let le_u e1 e2 = relop ty LeU e1 e2

  let ge_u e1 e2 = relop ty GeU e1 e2

  let of_int32 e = cvtop ty (Sign_extend 32) e

  let to_int32 e = cvtop (Ty_bitv 32) WrapI64 e

  let trunc_f32_s x = cvtop ty TruncSF32 x

  let trunc_f32_u x = cvtop ty TruncUF32 x

  let trunc_f64_s x = cvtop ty TruncSF64 x

  let trunc_f64_u x = cvtop ty TruncUF64 x

  let trunc_sat_f32_s _ = assert false

  let trunc_sat_f32_u _ = assert false

  let trunc_sat_f64_s _ = assert false

  let trunc_sat_f64_u _ = assert false

  let reinterpret_f64 x = cvtop ty Reinterpret_float x

  (* FIXME: This is probably wrong? *)
  let extend_s n x =
    cvtop ty (Sign_extend (64 - n)) (make (Extract (x, n / 8, 0)))

  let extend_i32_s x = cvtop ty (Sign_extend 32) x

  let extend_i32_u x = cvtop ty (Zero_extend 32) x
end

module F32 = struct
  let ty = Ty_fp 32

  let zero = const_f32 Float32.zero

  let abs x = unop ty Abs x

  let neg x = unop ty Neg x

  let sqrt x = unop ty Sqrt x

  let ceil x = unop ty Ceil x

  let floor x = unop ty Floor x

  let trunc x = unop ty Trunc x

  let nearest x = unop ty Nearest x

  let add x y = binop ty Add x y

  let sub x y = binop ty Sub x y

  let mul x y = binop ty Mul x y

  let div x y = binop ty Div x y

  let min x y = binop ty Min x y

  let max x y = binop ty Max x y

  let eq x y = relop ty Eq x y

  let ne x y = relop ty Ne x y

  let lt x y = relop ty Lt x y

  let gt x y = relop ty Gt x y

  let le x y = relop ty Le x y

  let ge x y = relop ty Ge x y

  let convert_i32_s x = cvtop ty ConvertSI32 x

  let convert_i32_u x = cvtop ty ConvertUI32 x

  let convert_i64_s x = cvtop ty ConvertSI64 x

  let convert_i64_u x = cvtop ty ConvertUI64 x

  let demote_f64 x = cvtop ty DemoteF64 x

  let reinterpret_i32 x = cvtop ty Reinterpret_int x

  let of_bits x = cvtop ty Reinterpret_int x

  let to_bits x = cvtop (Ty_bitv 32) Reinterpret_float x

  let copy_sign x y =
    let xi = to_bits (abs x) in
    let yi = to_bits y in
    of_bits @@ I32.logor xi (I32.logand yi (const_i32 Int32.min_int))
end

module F64 = struct
  let ty = Ty_fp 64

  let zero = const_f64 Float64.zero

  let abs x = unop ty Abs x

  let neg x = unop ty Neg x

  let sqrt x = unop ty Sqrt x

  let ceil x = unop ty Ceil x

  let floor x = unop ty Floor x

  let trunc x = unop ty Trunc x

  let nearest x = unop ty Nearest x

  let add x y = binop ty Add x y

  let sub x y = binop ty Sub x y

  let mul x y = binop ty Mul x y

  let div x y = binop ty Div x y

  let min x y = binop ty Min x y

  let max x y = binop ty Max x y

  let eq x y = relop ty Eq x y

  let ne x y = relop ty Ne x y

  let lt x y = relop ty Lt x y

  let gt x y = relop ty Gt x y

  let le x y = relop ty Le x y

  let ge x y = relop ty Ge x y

  let convert_i32_s x = cvtop ty ConvertSI32 x

  let convert_i32_u x = cvtop ty ConvertUI32 x

  let convert_i64_s x = cvtop ty ConvertSI64 x

  let convert_i64_u x = cvtop ty ConvertUI64 x

  let promote_f32 x = cvtop ty PromoteF32 x

  let reinterpret_i64 x = cvtop ty Reinterpret_int x

  let of_bits x = cvtop ty Reinterpret_int x

  let to_bits x = cvtop (Ty_bitv 64) Reinterpret_float x

  let copy_sign x y =
    let xi = to_bits (abs x) in
    let yi = to_bits y in
    of_bits @@ I64.logor xi (I64.logand yi (const_i64 Int64.min_int))
end
