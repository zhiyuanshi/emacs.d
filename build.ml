#!/usr/bin/env ocamlscript
Ocaml.packs      := ["core"; "async"];;
Ocaml.ocamlflags := ["-thread"; "-annot"];;
--
open Core.Std

let get_basename filename =
  if not (Sys.is_file_exn filename) then
    if not (Sys.is_file_exn (filename ^ ".ml")) then
      Error "File not found"
    else
      Ok filename
  else
    match String.rsplit2 filename ~on:'.' with
    | None ->
      Error "Incorrect file extention; '*.ml' or '*.mli' expected"
    | Some (basename, ext) ->
      match ext with
      | "ml" ->
        Ok basename
      | "mli" ->
        if Sys.is_file_exn (basename ^ ".ml") then
          Ok basename
        else
          Error "Missing the corresponding .mli file"
      | _ ->
          Error "Incorrect file extention; '*.ml' or '*.mli' expected"

let build basename =
  Sys.command ("ocamlbuild -use-ocamlfind " ^ basename ^ ".native")

let run basename =
  Sys.command ("./" ^ basename ^ ".native")

let main r filename () =
  match get_basename filename with
  | Error msg   -> eprintf "Error: %s\n" msg
  | Ok basename ->
    match r with
    | false -> if build basename <> 0 then () else printf "Build successful\n"
    | true  ->
      if not (Sys.is_file_exn (basename ^ ".native")) then
        eprintf
          "Native code file not found; maybe you forget to build it first.\n"
      else
        ignore (run basename)

let command =
  Command.basic
    ~summary:"Build or run OCaml source code"
    ~readme:(fun () -> "Just ask George at the moment...")
    Command.Spec.(
      empty
      +> flag "-r" no_arg
          ~doc:" Run native code if it exists"
      +> anon ("filename" %: string)
    )
    main

let () = Command.run command
