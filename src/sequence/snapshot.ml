(* This module provides functions to parse a sequence of snapshots (trace) from JSON format.
 * The trace structure consists of events, each containing elements with positions and regions.
 *)

open Yojson.Basic.Util

(* Trace Strucure*)
type position =
  {x: float; y: float; z: float; yaw: float; pitch: float; roll: float}

type region = {region_type: string; radius: float}

type element =
  {id: int; element_type: string; position: position; region: region}

type event = {event_id: int; timestamp: string; elements: element list}

type trace = {trace: event list}

(* Helpers to handle parsing*)
let to_number json =
  try to_float json
  with Yojson.Basic.Util.Type_error _ -> float_of_int (to_int json)

let to_int_or_string json =
  try to_int json
  with Yojson.Basic.Util.Type_error _ -> int_of_string (to_string json)

(* Function to parse a position *)
let parse_position json =
  { x= json |> member "x" |> to_number
  ; y= json |> member "y" |> to_number
  ; z= json |> member "z" |> to_number
  ; yaw= json |> member "yaw" |> to_number
  ; pitch= json |> member "pitch" |> to_number
  ; roll= json |> member "roll" |> to_number }

(* Function to parse region *)
let parse_region json =
  { region_type= json |> member "type" |> to_string
  ; radius= json |> member "radius" |> to_number }

(* Function to parse an element*)
let parse_element json =
  { id= json |> member "ID" |> to_int_or_string
  ; element_type= json |> member "type" |> to_string
  ; position= json |> member "position" |> parse_position
  ; region= json |> member "region" |> parse_region }

(* Function to parse an event*)
let parse_event json =
  { event_id= json |> member "eventID" |> to_int_or_string
  ; timestamp= json |> member "timestamp" |> to_string
  ; elements= json |> member "elements" |> to_list |> List.map parse_element
  }

(* Function to parse the trace *)
let parse_trace json =
  {trace= json |> member "trace" |> to_list |> List.map parse_event}
