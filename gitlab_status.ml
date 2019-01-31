open Core
open Async

exception Missing_variables of string

type gitlab_event = {id: int; status: string}

let usage =
  {|
The environment variables

GITLAB_URI
PROJECT_ID
PRIVATE_TOKEN

must be set before running the program.
|}

let die () = raise (Missing_variables usage)

let getenv variable =
  match Sys.getenv variable with
  | Some value -> value
  | None -> print_endline usage ; die ()

let gitlab_uri = getenv "GITLAB_URI"

let project_id = getenv "PROJECT_ID"

let private_token = getenv "PRIVATE_TOKEN"

let query_uri =
  let base_uri =
    Uri.of_string
      (gitlab_uri ^ "/api/v4/projects/" ^ project_id ^ "/pipelines?ref=master")
  in
  Uri.add_query_param base_uri ("private_token", [private_token])

let parse_json string =
  let accumulator result json =
    Result.(
      result
      >>= fun list ->
      match json with
      | `Assoc kv_list ->
          let find = List.Assoc.find_exn ~equal:String.equal kv_list in
          let id = find "id" |> Yojson.Basic.to_string |> int_of_string in
          let status = find "status" |> Yojson.Basic.to_string in
          let event = {id; status} in
          Ok (event :: list)
      | _ -> Error "Encountered an unexpected json value in the list")
  in
  match Yojson.Basic.from_string string with
  | `List list -> List.fold ~init:(Result.Ok []) ~f:accumulator list
  | _ -> Result.Error "Expected the json data to be a list of events"

let find_latest_event parse_result =
  Result.(
    parse_result
    >>= fun event_list ->
    match
      List.max_elt ~compare:(fun a b -> Int.compare a.id b.id) event_list
    with
    | Some event -> Result.Ok event
    | None ->
        Result.Error
          "Failed when trying to find the latest event. Maybe the list of \
           events is empty?")

let main () =
  Cohttp_async.Client.get query_uri
  >>= (fun (_, body) -> Cohttp_async.Body.to_string body)
  >>| parse_json >>| find_latest_event
  >>= fun result ->
  match result with
  | Error message ->
      prerr_endline ("Something went wrong: " ^ message) ;
      exit 1
  | Ok event ->
      print_endline
        ("id: " ^ string_of_int event.id ^ ", status: " ^ event.status) ;
      exit 0

let () =
  Command.async_spec ~summary:"Retrieve pipeline status from gitlab"
    Command.Spec.empty main
  |> Command.run
