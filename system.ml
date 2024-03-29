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

exception OSError of string
exception InternalError of string
exception NotADirectory of Path.root

include Sys
      
let xopen str : unit =
  if (Open.in_default_app str) then () else
    raise (OSError ("Could not open "^str^"^!"))
  
let open_file (path : Path.root) : unit =
  xopen (Path.string_of_root path)
  
let open_url (url : string) : unit =
  xopen url
  
(* Get files recursively *)
let get_files ?(hidden=false) (path : Path.root) : Path.root Seq.t =
  if (not (Sys.is_directory (Path.string_of_root path))) then
    raise (NotADirectory path);
  let rec get_files_ path =
    (if (not hidden) && (Path.hidden path) then (Seq.empty) else
       (if (Sys.is_directory (Path.string_of_root path)) then
          (Seq.concat_map (fun name ->
               let name = Path.mk_name name in
               get_files_ (Path.merge_lst path [name]))
             (Array.to_seq (Sys.readdir (Path.string_of_root path))))
        else (Seq.return path)))
  in (get_files_ path)

(* Remove directory recursively *)
let rmdir (path : Path.root) : unit =
  if (not (Sys.is_directory (Path.string_of_root path))) then
    raise (NotADirectory path);
  let rec rmdir_ path =
    (if (Sys.is_directory (Path.string_of_root path)) then
       ((Seq.iter (fun name ->
             (rmdir_ (Path.merge_lst path [Path.mk_name name])))
           (Array.to_seq (Sys.readdir (Path.string_of_root path))));
        Sys.rmdir (Path.string_of_root path))
     else
       (Sys.remove (Path.string_of_root path)))
  in (rmdir_ path)

let remove (path : Path.root) : unit =
  Sys.remove (Path.string_of_root path)
  
let make_dirp (root : Path.root) : unit =
  let root = Path.drop_leaf root in
  FileUtil.mkdir ~parent:true (Path.string_of_root root)

let make_dirp_leaf (root : Path.root) : unit =
  FileUtil.mkdir ~parent:true (Path.string_of_root root)

let move (path : Path.root) (new_path : Path.root) : unit =
  FileUtil.mv (Path.string_of_root path) (Path.string_of_root new_path)
    
let file_exists (path : Path.root) : bool =
  Sys.file_exists (Path.string_of_root path)

let empty_dir (path : Path.root) : bool =
  List.of_seq (get_files path) = []

let getenv_opt = Sys.getenv_opt
