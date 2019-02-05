open Core
open Async

type gitlab_event = {id: int; status: string}

let usage =
  {|
The environment variables

GITLAB_URI
PROJECT_ID
PRIVATE_TOKEN

must be set before running the program.
|}

let query_uri gitlab_uri project_id private_token =
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

type config = {gitlab_uri: string; project_id: string; private_token: string}

let main () =
  let config =
    Option.(
      Sys.getenv "GITLAB_URI"
      >>= fun gitlab_uri ->
      Sys.getenv "PROJECT_ID"
      >>= fun project_id ->
      Sys.getenv "PRIVATE_TOKEN"
      >>= fun private_token -> return {gitlab_uri; project_id; private_token})
  in
  match config with
  | Some {gitlab_uri; project_id; private_token} -> (
      let query_uri = query_uri gitlab_uri project_id private_token in
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
          exit 0 )
  | _ -> prerr_endline usage ; exit 1

let () =
  Command.async_spec ~summary:"Retrieve pipeline status from gitlab"
    Command.Spec.empty main
  |> Command.run
