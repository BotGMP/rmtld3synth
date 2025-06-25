(* rmtld3synth tool *)
open Sexplib
open Sexplib.Conv
open Rmtld3
open Helper
open Synthesis
open Interface
open Interface.Rmdslparser
open Dsl

open Ltlxms
open Trace

let helper = mk_helper

let ltlxms_lang = ref ""

let smtlibv2_lang = ref false

let simplify_formula = ref false

let cpp11_lang = ref false

let ocaml_lang = ref false

let spark2014_lang = ref false

let smt_solver = ref ""

let solver_statistics_flag = ref false

let get_schedule_flag = ref false

let trace_style = ref ""

let gen_rmtld_formula = ref false

let rtmlib2_period = ref 0

let set_rtm_config_file v = set_setting "rtm_config_file" (Txt v) helper

let set_smt_formula f = smtlibv2_lang := true

let set_simplify_formula f = simplify_formula := true

let set_ocaml_language f = ocaml_lang := true

let set_cpp_language f = cpp11_lang := true

let set_spark2014_language f = spark2014_lang := true

let set_solve_statistics f = solver_statistics_flag := true

let set_get_schedule f = get_schedule_flag := true

let set_trace_style f = trace_style := f

let set_gen_rmtld_formula f = gen_rmtld_formula := true

let set_eval _ = set_setting "evaluate" (Sel true) helper

let set_env v =
  let json =
    (* check whether this is a filename or a json string *)
    let re_path_unix = Str.regexp {|^\([\.]?/[^/ ]*\)+/?$|} in
    let re_path_windows =
      Str.regexp
        {|^[a-zA-Z]:\(\\[a-zA-Z0-9_\.-]+\)*\([\\]\|[\.][a-zA-Z]+\)?$|}
    in
    if
      Str.string_match re_path_unix v 0
      || Str.string_match re_path_windows v 0
    then Yojson.Safe.from_file v
    else Yojson.Safe.from_string v
  in
  let x = json |> Yojson.Safe.to_string in
  set_setting "environment" (Txt x) helper

(* output settings *)
let set_out_file v = set_setting "out_file" (Txt v) helper

let set_out_dir v = set_setting "out_dir" (Txt v) helper

(* input settings *)
let set_exp v =
  set_setting "input_exp_sexp" (Txt v) helper ;
  set_setting "input_exp" (Fm (formula_of_sexp (Sexp.of_string v))) helper

let set_ltlxms v =
  ltlxms_lang := v;
  (* dummy formula to bypass the check and use ltlxms *)
  set_setting "input_exp" (Fm (Prop "a")) helper

let set_exp_dsl v =
  set_setting "input_exp_dsl" (Txt v) helper ;
  set_setting "input_exp"
    (Fm (Dsl.Load.parse_string v |> Dsl.TranslateToRmtld3.conv_fm))
    helper

let set_exp_ltxeq v =
  set_setting "input_exp_ltxeq" (Txt v) helper ;
  set_setting "input_exp" (Fm (Tex.Texeqparser.texeqparser v)) helper

let set_exp_rmdsl v =
  set_setting "input_exp_rmdsl" (Txt v) helper ;
  let lst =
    Rmdslparser.rmtld3_fm_lst_of_rmdsl_lst (Rmdslparser.rmdslparser v)
  in
  List.iter (fun a -> set_setting "input_exp" (Fm a) helper) lst

(* rtm settings *)
let set_rtm_period v = set_setting "rtm_period" (Num v) helper

let set_rtm_buffer_size v = set_setting "rtm_buffer_size" (Num v) helper

let set_rtm_min_inter_arrival_time v =
  set_setting "rtm_min_inter_arrival_time" (Num v) helper

let set_rtm_max_period v = set_setting "rtm_max_period" (Num v) helper

let set_rtm_event_type v = set_setting "rtm_event_type" (Txt v) helper

let set_rtm_event_subtype v = set_setting "rtm_event_subtype" (Txt v) helper

let set_rtm_monitor_name_prefix v =
  set_setting "rtm_monitor_name_prefix" (Txt v) helper

let set_rtm_monitor_time_unit v =
  if v = "ns" || v = "us" || v = "ms" || v = "s" then
    set_setting_replace "rtm_monitor_time_unit" (Txt v) helper
  else failwith "check if time units are 'ns', 'us', 'ms', or 's'."

(* general settings *)
let set_gen_tests v = set_setting "gen_tests" (Sel v) helper

(* default settings *)
let default_settings helper =
  let default_settings =
    "(rtm_period 200000)\n\
     (rtm_buffer_size 100)\n\
     (rtm_min_inter_arrival_time 1)\n\
     (rtm_max_period 2000000)\n\
     (rtm_monitor_name_prefix rtm_#_%)\n\
     (rtm_monitor_time_unit s)\n\
     (gen_tests false)"
  in
  (* apply settings *)
  apply_settings default_settings helper

open Unix
open Sexplib
open Sexplib.Conv
open Synthesis
open Synthesis.Simplify
open Synthesis.Smtlib2
open Synthesis.Cpp11
open Synthesis.Ocaml
open Synthesis.Spark2014
open Interface.Z3solver_

let set_recursive_unrolling arg =
  (* check scope: auto or [0-9]+ *)
  if arg <> "auto" && not (Str.string_match (Str.regexp "[0-9]+") arg 0) then
    failwith ("Unrecognized recursive unrolling parameter '" ^ arg ^ "'.")
  else if Str.string_match (Str.regexp "[0-9]+") arg 0 then
    Smtlib2.recursive_unrolling_depth := int_of_string arg ;
  Smtlib2.enable_recursive_unrolling ()
(*Smt.recursive_unrolling := true*)

let set_assumption_unary_sequence () = Smtlib2.assume_unary_sequence := true

(* swap to smtlib module only *)
let set_solve_z3 f = Smtlib2.set_solver Z3
(*Smt.solver := "z3"*)

(* swap to smtlib module only *)
let set_solve_cvc4 f = Smtlib2.set_solver CVC4
(*Smt.solver := "cvc4"*)

(* swap to smtlib module only *)
let chose_synthesis a b c =
  if !cpp11_lang then a ()
  else if !ocaml_lang then b ()
  else if !spark2014_lang then c ()

(** Calls and configures the synthesis of ocaml and cpp monitors. *)
let synth_monitor fm =
  (* helper that is used to query configuration settings of rmtld3synth. *)
  try
    let a, b, c =
      settings
        (settings_from_file (get_setting_string "rtm_config_file" helper))
    in
    (* load settings and formulas from the configuration file! *)
    List.iter (fun (a, b) -> set_setting a (Num b) helper) a ;
    List.iter (fun (a, b) -> set_setting a (Txt b) helper) b ;
    List.iter (fun (_, _, b) -> set_setting "input_exp" (Fm b) helper) c
  with _ ->
    () ;
    set_setting "version" (Txt Version.git) helper ;
    let create_dir dir_name =
      try
        let state = Sys.is_directory dir_name in
        if state then () else Unix.mkdir dir_name 0o777
      with _ -> Unix.mkdir dir_name 0o777
    in
    (* TODO: not clear what it does 'event_queue_size' but it has been adapted! *)
    (* monitor synthesis setting *)
    let event_queue_size =
      max
        (get_setting_int "rtm_buffer_size" helper)
        (* compute the buffer size based on inter-arrival time *)
        ( List.fold_left
            (fun v formula ->
              let value = int_of_float (calculate_t_upper_bound formula) in
              if value > v then value else v )
            0
            (get_all_setting_formula "input_exp" helper)
        / get_setting_int "rtm_min_inter_arrival_time" helper )
    in
    verb (fun _ ->
        Printf.printf "Buffer is defined as length %d\n" event_queue_size ) ;
    (* if setting out_dir is available, try to create it *)
    ( try
        if get_setting_string "out_dir" helper <> "" then
          create_dir (get_setting_string "out_dir" helper)
      with _ -> () ) ;
    (* Functors for synthesis *)
    let module Conv_cpp11 = Standard.Translate (Synthesis.Cpp11) in
    let module Conv_ocaml = Standard.Translate (Synthesis.Ocaml) in
    let module Conv_spark2014 = Standard.Translate (Synthesis.Spark2014) in
    (* Chose syntheis for constructing monitors with a certain name and
       period *)
    chose_synthesis
      (fun _ ->
        (* performs monitor synthesis for cpp11 *)
        synth_cpp11 Conv_cpp11.synth helper )
      (fun _ ->
        (* performs monitor synthesis for ocaml *)
        synth_ocaml Conv_ocaml.synth helper )
      (fun _ ->
        (* performs monitor synthesis for spark2014 *)
        synth_spark2014 Conv_spark2014.synth helper )

(** Formulates and configures the synthesis of rmtld into smtlibv2. *)
let synth_sat_problem formula =
  (* settings *)
  if !Smtlib2.recursive_unrolling_depth = 0 then
    Smtlib2.recursive_unrolling_depth :=
      int_of_float
        (try calculate_t_upper_bound formula with Failure _ -> 25.) ;
  if !Smtlib2.recursive_unrolling_depth = 0 then
    (* to force if not zero *)
    Smtlib2.recursive_unrolling_depth := 1 ;
  verb (fun _ ->
      print_endline
        ("t_upper_bound" ^ string_of_int !Smtlib2.recursive_unrolling_depth) ) ;
  (* Functor to translate rmtld3 into smtlibv2 *)
  let module Smtlib = Standard.Translate (Smtlib2) in
  (* 'smtlib2_str' will contain the output of the translation *)
  let smtlib2_str = synth_smtlib Smtlib.synth formula helper in
  (*let smtlib2_str = rmtld3synthsmt formula helper in*)
  if isZ3SolverEnabled () then (
    verb (fun _ -> print_endline "Z3 solver enabled.") ;
    let ctx, exp = parse_smtlibv2 smtlib2_str in
    let out, solver = solve_ ctx exp in
    verb (fun _ -> print_endline ("Result: " ^ out)) ;
    if not !get_schedule_flag then print_endline out ;
    if out = "satisfiable" then (
      let model = get_model ctx solver in
      if not !get_schedule_flag then print_endline (string_of_z3model model) ;
      if !get_schedule_flag then (
        let scheduler_trace = get_scheduler ctx model helper in
        if !trace_style = "tinterval" then
          let _, trc_str =
            List.fold_left
              (fun (cnt, a) b ->
                let cnte = cnt +. 1. in
                (cnte, a ^ " (\"" ^ b ^ "\",(" ^ string_of_float cnt ^ ")); ")
                )
              (0., "") scheduler_trace
          in
          print_endline trc_str
        else if !trace_style = "tcum" then
          let _, trc_str =
            List.fold_left
              (fun (cnt, a) b ->
                let cnte = cnt +. 1. in
                ( cnte
                , a ^ " (\"" ^ b ^ "\","
                  ^ string_of_float (cnte -. cnt)
                  ^ "); " ) )
              (0., "") scheduler_trace
          in
          print_endline trc_str
        else
          print_endline
            (Sexp.to_string (sexp_of_trace_untimed scheduler_trace)) ;
        () ) ) ) ;

open Version
open Helper

(** rmtld3synth's command line interface *)
let _ =
  let speclist =
    [ (* action flags *)
      ( "--gen-rmtld-formula"
      , Arg.Unit set_gen_rmtld_formula
      , " Call `gen_formula_default` function" )
    ; ( "--synth-smtlibv2"
      , Arg.Unit set_smt_formula
      , " Enables synthesis for SMT-LIBv2 language\n\n\
        \ Flags for runtime monitoring (rtm) synthesis: " )
    ; ( "--synth-ocaml"
      , Arg.Unit set_ocaml_language
      , " Enables synthesis for Ocaml language" )
    ; ( "--synth-cpp11"
      , Arg.Unit set_cpp_language
      , " Enables synthesis for C++11 language" )
    ; ( "--synth-spark2014"
      , Arg.Unit set_spark2014_language
      , " Enables synthesis for Spark2014 language (Experimental)\n\n\
        \ Flags for solving: " )
    ; ( "--simpl-cad"
      , Arg.Unit set_simplify_formula
      , " Simplify quantified RMTLD formulas using CAD (Experimental)" )
    ; ( "--solver-z3"
      , Arg.Unit set_solve_z3
      , " Enables solving smtlibv2 problems using Z3 SMT solver" )
    ; ( "--solver-cvc4"
      , Arg.Unit set_solve_cvc4
      , " Enables solving smtlibv2 problems using cvc4 SMT solver" )
    ; ( "--rec-unrolling"
      , Arg.String set_recursive_unrolling
      , " Enables recursive unrolling with depth: auto, [0-9]+" )
    ; ( "--assume-unary-seq"
      , Arg.Unit set_assumption_unary_sequence
      , " Assume that the output sequence is unary." )
    ; ( "--solver-statistics"
      , Arg.Unit set_solve_statistics
      , " Enables printing the solver statistics" )
    ; ("--get-trace", Arg.Unit set_get_schedule, " Returns the schedule")
    ; ( "--trace-style"
      , Arg.String set_trace_style
      , " Sets the trace style\n\n Evaluation:" )
    ; (* evaluate formula on a given environment *)
      ( "--eval"
      , Arg.Unit set_eval
      , " Enables evaluation of a formula on an environment" )
    ; (* input environments *)
      ( "--include"
      , Arg.String set_env
      , " Includes a given environment (e.g., --include 'filename.env')\n\n\
        \ Input:" )
    ; (* input expressions *)
      (* --input-ltlxms*)
      ( "--input-sexp"
      , Arg.String set_exp
      , " Inputs sexp expression (RMTLD3 formula)" )
    ;
    ( "--input-ltlxms"
      , Arg.String set_ltlxms 
      , " Inputs ltlxms expression (RMTLD3 formula)" )
    ; ( "--input-dsl"
      , Arg.String set_exp_dsl
      , " Inputs dsl expression (RMTLD3 formula)" )
    ; ( "--input-latexeq"
      , Arg.String set_exp_ltxeq
      , " Inputs latex equation expressions (RMTLD3 formula) (Experimental)"
      )
    ; ( "--input-rmdsl"
      , Arg.String set_exp_rmdsl
      , " Inputs rmdsl expressions for schedulability analysis \
         (Experimental)\n\n\
        \ Set runtime monitoring (rtm) settings:" )
    ; (* exclusively used for monitoring synthesis *)
      ( "--config-file"
      , Arg.String set_rtm_config_file
      , " Set settings from a file" )
    ; (* setup settings for rtm *)
      ( "--set-monitor-period"
      , Arg.Int set_rtm_period
      , " Set monitoring period" )
    ; ("--set-buffer-size", Arg.Int set_rtm_buffer_size, " Set buffer size")
    ; ( "--set-min-inter-time"
      , Arg.Int set_rtm_min_inter_arrival_time
      , " Set minimum inter arrival time" )
    ; ("--set-max-period", Arg.Int set_rtm_max_period, " Set maximum period")
    ; ("--set-event-type", Arg.String set_rtm_event_type, " Set event type")
    ; ( "--set-event-subtype"
      , Arg.String set_rtm_event_subtype
      , " Set event subtype" )
    ; ( "--set-monitor-name-prefix"
      , Arg.String set_rtm_monitor_name_prefix
      , " Set monitor name prefix" )
    ; ( "--set-monitor-time-unit"
      , Arg.String set_rtm_monitor_time_unit
      , " Set monitor time units\n\n Output:" )
    ; (*output models *)
      ( "--out-file"
      , Arg.String set_out_file
      , " Set the output filename for synthesis" )
    ; ( "--out-src"
      , Arg.String set_out_dir
      , " Set the output directory for synthesis" )
    ; ( "--out-dir"
      , Arg.String set_out_dir
      , " Set the output directory for synthesis\n\n Options:" )
    ; ("--verbose", Arg.Set_int verb_mode, " Enables verbose mode")
    ; ( "--version"
      , Arg.Unit
          (fun () ->
            print_endline ("Git version " ^ Version.git) ;
            exit 0 )
      , " Version and SW information\n" ) ]
  in
  default_settings helper ;
  let usage_msg =
    "rmtld3synth flags [options] input [output]\n\n Flags for synthesis: "
  in
  ( try Arg.parse_argv Sys.argv (Arg.align speclist) print_endline usage_msg
    with Arg.Help msg | Arg.Bad msg -> print_endline msg ) ;
  verb_m 2 (fun a -> print_endline (Version.git ^ "\n")) ;
  verb_m 1 (fun _ ->
      print_endline "Current Configuration:" ;
      print_settings helper ) ;
  let expressions = get_all_setting_formula "input_exp" helper in
  if expressions = [] then (
    print_endline "no formula is available." ;
    exit 1 ) ;
  verb_m 1 (fun _ ->
      print_endline "Expression(s) selected to encode:" ;
      List.iter
        (fun exp -> print_endline (string_of_rmtld_fm exp))
        expressions ) ;
  (* selects the type of the input formula *)
  let input_fm =
    if expressions <> [] then List.hd expressions
    else (* there is no imput formula *)
      mfalse
  in
  let to_simplify fm : rmtld3_fm =
    if !simplify_formula then (
      let smp = simplify fm in
      verb (fun _ ->
          print_endline "Output formula from the simplification process:\n" ;
          print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm smp)) ;
          print_endline
            "--------------------------------------------------------------------------------\n" ) ;
      smp )
    else fm
  in
  (* Selects synthesis for smtlibv2, ocaml, cpp or does simplification. *)
  if !smtlibv2_lang <> false then
    if input_fm <> mfalse then synth_sat_problem (to_simplify input_fm)
    else (
      verb (fun _ -> print_endline "Rmdsl parsing enabled.") ;
      let fm_lst =
        get_all_setting_formula "input_exp"
          helper (* list of expressions with input_exp tag *)
      in
      verb (fun _ ->
          print_endline
            "--------------------------------------------------------------------------------\n" ;
          print_endline "rmtld3 formula(s): " ;
          print_endline
            ("Available goals: " ^ string_of_int (List.length fm_lst)) ) ;
      let _ =
        List.fold_left
          (fun a fm_goal ->
            synth_sat_problem (to_simplify fm_goal) ;
            a + 1 )
          1 fm_lst
      in
      () )
  (* rmtld3synth --input-ltlxms <nome fich.json> --eval --include <nome fich trace> --solver-z3 *)
  else if !ltlxms_lang <> "" then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for LTLXMS language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
        let property_file = !ltlxms_lang in
        let trace_file =
          try get_setting_string "environment" helper
          with _ -> ""
        in
        if property_file = "" || trace_file = "" then
          Printf.eprintf "Error: Both --input-ltlxms <property.json> and --include <trace.json> must be provided.\n"
        else (
          (* Temp file if none exists*)
          let trace_path =
            if Sys.file_exists trace_file then trace_file
            else (
              let tmp = Filename.temp_file "trace" ".json" in
              let oc = open_out tmp in
              output_string oc trace_file;
              close_out oc;
              tmp
            )
          in
          (* Trace.parse_trace *)
          (* Ltlxms.formula_to_z3 *)
          let cmd =
            Printf.sprintf
              "dune exec src/ltlxms/test/test_until_trace.exe %s %s"
              trace_path property_file
          in
          let status = Sys.command cmd in
          if status <> 0 then Printf.eprintf "Error: test_until_trace failed\n"
        )
  )

  else if !ocaml_lang then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for Ocaml language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
    synth_monitor input_fm )
  else if !cpp11_lang then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for C++11 language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
    let default_cpp11_settings =
      "(rtm_event_type Event)\n\
       (rtm_event_subtype std::underlying_type<_auto_gen_prop>::type)"
    in
    (* apply settings *)
    apply_settings default_cpp11_settings helper ;
    synth_monitor input_fm )
  else if !spark2014_lang then (
    verb_m 1 (fun _ ->
        print_endline "Synthesis for SPARK 2014 language" ;
        print_endline
          "--------------------------------------------------------------------------------\n" ) ;
    synth_monitor input_fm )
  else if !simplify_formula then
    let inn = input_fm in
    if inn <> mfalse then
      let smp = to_simplify inn in
      slatex_of_rmtld_fm smp |> print_endline
    else raise (Failure "Cannot simplify the specified input.")
  else if !gen_rmtld_formula then
    let fm = gen_formula_default () in
    slatex_of_rmtld_fm fm |> print_endline
  else if get_setting_bool "evaluate" helper then
    (* get trc *)
    let json =
      get_setting_string "environment" helper |> Yojson.Safe.from_string
    in
    let json_trc = json |> Yojson.Safe.Util.member "trc" in
    let json_t = json |> Yojson.Safe.Util.member "t" in
    let trc =
      if json_trc <> `Null then trace_of_yojson json_trc
      else failwith "No 'trc' available!"
    in
    let env = Rmtld3.environment trc in
    let lg_env = Rmtld3.lenv in
    let t = if json_t <> `Null then Rmtld3.time_of_yojson json_t else 0. in
    let res = Rmtld3.eval (env, lg_env, t) input_fm in
    res |> b3_to_string |> print_endline
  else print_endline "Nothing to do. Type --help"
