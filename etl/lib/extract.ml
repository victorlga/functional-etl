open Parse

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

(* https://ocaml.org/cookbook/http-get-request/cohttp-lwt-unix *)
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

let extract_csv_from_web url filepath=
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
    Extracts order and order item data from CSV files.
    
    @param orders_filepath Path to the CSV file containing order data.
    @param order_items_filepath Path to the CSV file containing order item data.
    @return A tuple containing lists of parsed orders and order items.
    @raise Sys_error If file operations fail.
    @raise Csv.Error If the CSV format is invalid.
    @raise Failure If parsing of order or order item data fails.
*)
let extract orders_filepath order_items_filepath =
  let url = "https://raw.githubusercontent.com/victorlga/etl-ocaml/refs/heads/main/" in
  extract_csv_from_web url orders_filepath ;
  extract_csv_from_web url order_items_filepath ;

  let raw_orders = extract_csv_from_file orders_filepath in
  let orders = List.map parse_order raw_orders in
  let raw_order_items = extract_csv_from_file order_items_filepath in
  let order_items = List.map parse_order_item raw_order_items in
  (orders, order_items)
