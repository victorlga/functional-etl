let ( let* ) = Result.bind


let extract_csv_from_file filepath =
  try
    let data = Csv.Rows.load ~has_header:true filepath in
    Ok data
  with
  | exn -> Error (Printexc.to_string exn)

(**
    @author Source: OCaml Cookbook (https://ocaml.org/cookbook/http-get-request/cohttp-lwt-unix)
*)
let http_get url =
  let ( let* ) = Lwt.bind in
  let* (resp, body) =
    Cohttp_lwt_unix.Client.get (Uri.of_string url)
  in
  let code = resp
              |> Cohttp.Response.status
              |> Cohttp.Code.code_of_status in
  if Cohttp.Code.is_success code
  then
    let* b = Cohttp_lwt.Body.to_string body in
    Lwt.return (Ok b)
  else
    Lwt.return (Error (
      Cohttp.Code.reason_phrase_of_code code
    ))


let get_and_load_csv_from_web url filepath =
  let ( let* ) = Lwt.bind in
  Lwt_main.run (
    let full_url = url ^ filepath in
    let* result = http_get full_url in
    match result with
    | Error str -> Lwt.return (Error str)
    | Ok result ->
        let* () = 
          Lwt_io.with_file ~mode:Lwt_io.Output filepath (fun oc ->
            Lwt_io.write oc result
          )
        in
        Lwt.return (Ok ())
  )


let extract filepaths =
  let (orders_filepath, order_items_filepath) = filepaths in
  let url = "https://raw.githubusercontent.com/victorlga/etl-ocaml/refs/heads/main/" in
  let* _ = get_and_load_csv_from_web url orders_filepath in
  let* _ = get_and_load_csv_from_web url order_items_filepath in
  let* orders = extract_csv_from_file orders_filepath in
  let* order_items = extract_csv_from_file order_items_filepath in
  Ok (orders, order_items)

