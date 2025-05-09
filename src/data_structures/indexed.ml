(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t =
  { index : int
  ; value : 'a
  }

let get v = v.value

let get_index v = v.index

let map f v = { index = v.index; value = f v.value }

let return index value = { index; value }

let has_index idx { index; _ } = idx = index

let get_at i values =
  match List.find_opt (has_index i) values with
  | None -> None
  | Some { value; _ } -> Some value

let list_to_array l =
  List.sort
    (fun { index = i1; value = _ } { index = i2; value = _ } ->
      Int.compare i1 i2 )
    l
  |> List.map (fun { value; _ } -> value)
  |> Array.of_list
