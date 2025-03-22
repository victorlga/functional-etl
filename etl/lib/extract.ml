(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Reads a list of strings from user input until an empty line is entered.
    @param prompt The prompt to display to the user before input
    @return A list of strings entered by the user in reverse order of input *)
let get_list_from_user prompt =
  print_string prompt;
  flush stdout;
  let rec loop acc =
    let input = input_line stdin in
    if input = "" then List.rev acc
    else loop (input :: acc)
  in
  loop []

(** Extracts CSV data from a file.
    @param filepath Path to the CSV file to be loaded
    @return Result containing CSV data on success or error string on failure *)
let extract_csv_from_file filepath =
  try
    let data = Csv.Rows.load ~has_header:true filepath in
    Ok data
  with
  | exn -> Error (Printexc.to_string exn)

(** Performs an HTTP GET request to retrieve content from a URL.
    @author Source: OCaml Cookbook (https://ocaml.org/cookbook/http-get-request/cohttp-lwt-unix)
    @param url The URL to fetch data from
    @return Lwt promise resolving to Result with response body string on success or error message on failure *)
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

(** Downloads CSV data from a web URL and saves it to a local file.
    @param url Base URL for the data source
    @param filepath Local file path where CSV will be saved
    @return Result indicating success (Ok ()) or failure (Error string) *)
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

(** Extracts data from CSV files, downloading them from the web if necessary.
    @param filepaths Tuple of file paths (orders_filepath, order_items_filepath)
    @return Result containing tuple of orders and order_items CSV data on success,
            or error string on failure *)
let extract filepaths =
  let (orders_filepath, order_items_filepath) = filepaths in
  let url = "https://raw.githubusercontent.com/victorlga/etl-ocaml/refs/heads/main/" in
  let* _ = get_and_load_csv_from_web url orders_filepath in
  let* _ = get_and_load_csv_from_web url order_items_filepath in
  let* orders = extract_csv_from_file orders_filepath in
  let* order_items = extract_csv_from_file order_items_filepath in
  Ok (orders, order_items)