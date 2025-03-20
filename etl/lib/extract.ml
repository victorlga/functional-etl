let ( let* ) = Lwt.bind

(**
    Loads CSV data from a file into a list of rows.
    
    @param filepath The path to the CSV file to load.
    @return A list of CSV rows, where each row is a record of column values.
    @raise Sys_error If the file cannot be opened or read.
    @raise Csv.Error If the CSV format is invalid.
*)
let extract_csv_from_file filepath =
  Csv.Rows.load ~has_header:true filepath

(**
    Performs an HTTP GET request to the specified URL and returns the response body.
    
    @param url The URL to send the GET request to.
    @return An Lwt promise resolving to Ok with the response body string on success,
            or Error with the error message on failure.
    @author Source: OCaml Cookbook (https://ocaml.org/cookbook/http-get-request/cohttp-lwt-unix)
*)
let http_get url =
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

(**
    Downloads a CSV file from a web URL and saves it locally.
    
    @param url The base URL where the CSV file is located.
    @param filepath The relative path to the CSV file and local destination path.
    @return Unit (side effect: saves file or prints error).
    @raise Sys_error If file writing operations fail.
*)
let extract_and_load_csv_from_web url filepath=
  Lwt_main.run (
    let full_url = url ^ filepath in
    let* result = http_get full_url in
    match result with
    | Error str ->
      Printf.printf "%s:fail\n" full_url;
      Printf.printf "Error: %s\n" str;
      Lwt.return ()
    | Ok result ->
      Lwt_io.with_file ~mode:Lwt_io.Output filepath (fun oc ->
        Lwt_io.write oc result
      )
  )


(**
    Downloads order and order item data from CSV files hosted online.
    
    @param orders_filepath Local path where the orders CSV will be saved and read from.
    @param order_items_filepath Local path where the order items CSV will be saved and read from.
    @return A tuple of (orders, order_items) raw data.
    @raise Sys_error If file download or read operations fail.
    @raise Csv.Error If the CSV files have an invalid format.
    @raise Failure If parsing of order or order item records fails due to invalid data.
*)
let extract filepaths =
  let (orders_filepath, order_items_filepath) = filepaths in
  let url = "https://raw.githubusercontent.com/victorlga/etl-ocaml/refs/heads/main/" in
  extract_and_load_csv_from_web url orders_filepath ;
  extract_and_load_csv_from_web url order_items_filepath ;

  let orders = extract_csv_from_file orders_filepath in
  let order_items = extract_csv_from_file order_items_filepath in
  (orders, order_items)
