(* js_helper *)
open Js_of_ocaml

module Sys = struct
  let explode s = s |> String.to_seq |> List.of_seq

  let rec console_arg_split s cm b =
    match s with
    | [] -> [ cm ]
    | a :: x ->
        if a = ' ' && b mod 2 = 0 then [ cm ] @ console_arg_split x "" b
        else
          console_arg_split x
            (cm ^ if a = '\"' then "" else String.make 1 a)
            (if a = '\"' then b + 1 else b)

  let getenv x = List.assoc x Js_of_ocaml.Url.Current.arguments

  let argv =
    Array.of_list (console_arg_split (explode (getenv "argv")) "" 0)

  let executable_name = argv.(0)
  let file_exists c = false
  let remove c = ()
  let time c = 0.
  let is_directory c = false
  let command x = 1
end

let output_buffer_ = Buffer.create 1000

let flush x =
  let module J = Js_of_ocaml.Js.Unsafe in
  let () =
    J.call (Js_of_ocaml.Js.Unsafe.pure_js_expr "postMessage") (Js_of_ocaml.Js.Unsafe.pure_js_expr "self")
      [| J.inject (Js_of_ocaml.Js.string (Buffer.contents output_buffer_)) |]
  in
  Buffer.clear output_buffer_

let print_string = Buffer.add_string output_buffer_
let print_char = Buffer.add_char output_buffer_
let print_newline () = print_char '\n'

let print_endline s =
  print_string (s ^ "\n");
  flush ()

let caml_ml_output_char = print_char

(*let printf fmt = Printf.bprintf output_buffer_ fmt
  module Printf = struct
      include Printf
      let printf fmt = Printf.bprintf output_buffer_ fmt
  end*)

let _ =
  Js_of_ocaml.Sys_js.create_file ~name:"/static/config/default"
    ~content:
      "(buffer_size 100)\n\
       (maximum_inter_arrival_time 1)\n\
       (maximum_period 2000000)\n\
       (event_type Event)\n\
       (event_subtype int)\n\
       (cluster_name mon1)";
  Array.iter print_endline Sys.argv;
  print_endline
    "--------------------------------------------------------------------------------\n"

(*print_endline (fst (List.hd Url.Current.arguments));
  print_endline (snd (List.hd Url.Current.arguments));*)
