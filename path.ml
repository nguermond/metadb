(******************************************************************************)
(* Metadb                                                                     *)
(* Copyright (C) 2022 Nathan Guermond                                         *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify it    *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation, either version 3 of the License, or (at your option)  *)
(* any later version.                                                         *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *)
(* for more details.                                                          *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License along    *)
(* with this program. If not, see <https://www.gnu.org/licenses/>.            *)
(*                                                                            *)
(******************************************************************************)

exception InvalidRootType of string
exception InvalidRelType of string
exception InvalidNameType of string
exception UnexpectedFileExtension of string
                           
module P = FilePath.DefaultPath


type root = P.filename
type rel = P.filename
type name = P.filename

let check = ref true
          
let ifcheck (b : bool) : bool = (not !check) || b
       
let mk_root (root : string) : root =
  if (ifcheck (P.is_valid root && (not (P.is_relative root)))) then root
  else raise (InvalidRootType root)

let mk_rel (rel : string) : rel =
  if (ifcheck (P.is_valid rel && P.is_relative rel)) then rel
  else raise (InvalidRelType rel)

let mk_name (name : string) : name =
  if (ifcheck (P.is_valid name && (P.basename name = name))) then name
  else raise (InvalidNameType name)
  
(* let mk_path (path : string list) : name list =
 *   if !debug then (List.map mk_name path)
 *   else path *)

let rec unroot (root : root) : root * rel =
  let dir = P.dirname root in
  let file = P.basename root in
  if (dir = root) then (dir,file) else
    let (root,rel) = unroot dir in
    (root, P.concat rel file)
    
let string_of_root (root : root) : string = root
let string_of_rel (rel : rel) : string = rel
let string_of_name (name : name) : string = name
                                     
let merge_lst (root : root) (path : name list) : root =
  P.make_absolute root (P.make_filename path)

let merge (root : root) (path : rel) : root =
  P.make_absolute root path

let split (path : rel) : name list =
  let rec split_ path names =
    let dir = P.dirname path in
    let name = P.basename path in
    if (dir = path) then names else
      split_ dir (name :: names)
  in split_ path []

let add_file_ext (ext : string) (root : root) : root =
  P.add_extension root ext
  
let remove_file_ext_rel (ext : string) (path : rel) : rel =
  if (P.check_extension path ext) then
    P.chop_extension path
  else raise (UnexpectedFileExtension ext)
    
let remove_file_ext (ext : string) (root : root) : root =
  remove_file_ext_rel ext root

let strip_root (root : root) (path : root) : rel =
  P.make_relative root path

let drop_leaf (root : root) : root =
  P.dirname root
  
let get_leaf_rel (path : rel) : name =
  P.basename path

let get_leaf (root : root) : name =
  P.basename root
  
let hidden (path : root) : bool =
  let leaf = P.basename path in
  leaf.[0] = '.'

let pp_root ppf (path : root) =
  Format.fprintf ppf "%s" (string_of_root path)

let pp_rel ppf (path : rel) =
  Format.fprintf ppf "%s" (string_of_rel path)
