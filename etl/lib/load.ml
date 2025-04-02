open Types

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Executes a function with an output channel, ensuring it is properly closed.
    @param filepath Path to the output file
    @param f Function to execute with the output channel
    @return Result of the function execution *)
let with_output_channel filepath f =
  let oc = open_out filepath in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)

(** Writes a list of items to a CSV file with a header.
    @param header CSV header string
    @param to_row Function to convert an item to a CSV row string
    @param items List of items to write
    @param filepath Destination file path
    @return Unit indicating successful write *)
let write_csv header to_row items filepath =
  with_output_channel filepath (fun oc ->
    output_string oc header;
    List.iter (fun item -> output_string oc (to_row item ^ "\n")) items)

(** Writes order totals to a CSV file.
    @param order_totals List of order total records
    @param filepath Destination CSV file path
    @return Result indicating success (Ok ()) or failure (Error string) *)
let write_order_totals_to_csv order_totals filepath =
  let header = "order_id,total_amount,total_tax\n" in
  let to_row ot = 
    Printf.sprintf "%d,%.2f,%.2f"
      ot.order_id
      ot.total_amount
      ot.total_tax
  in
  write_csv header to_row order_totals filepath;
  Ok ()

(** Writes financial records to a CSV file.
    @param financial_records List of financial record entries
    @param filepath Destination CSV file path
    @return Result indicating success (Ok ()) or failure (Error string) *)
let write_financial_records_to_csv financial_records filepath =
  let header = "order_id,total_amount,total_tax\n" in
  let to_row fr = 
    Printf.sprintf "%s,%.2f,%.2f"
      fr.period
      fr.avg_rev
      fr.avg_tax
  in
  write_csv header to_row financial_records filepath;
  Ok ()

(** Creates a database table, dropping it first if it exists.
    @param db SQLite database handle
    @param table_name Name of the table to create
    @param drop_sql SQL command to drop the table
    @param create_sql SQL command to create the table
    @return Result indicating success (Ok ()) or failure (Error string) *)
let create_table db table_name drop_sql create_sql =
  ignore (Sqlite3.exec db drop_sql);
  match Sqlite3.exec db create_sql with
  | Sqlite3.Rc.OK -> Ok ()
  | rc -> Error (Printf.sprintf "Failed to create %s: %s" 
                  table_name (Sqlite3.Rc.to_string rc))

(** Executes a function within a database transaction.
    @param db SQLite database handle
    @param f Function to execute within the transaction
    @return Result of the function execution or error string on failure *)
let with_transaction db f =
  Sqlite3.exec db "BEGIN TRANSACTION" |> ignore;
  try
    let result = f db in
    Sqlite3.exec db "COMMIT" |> ignore;
    Ok result
  with e ->
    Sqlite3.exec db "ROLLBACK" |> ignore;
    Error (Printexc.to_string e)

(** Creates the order_totals table in SQLite.
    @param db SQLite database handle
    @return Result indicating success (Ok ()) or failure (Error string) *)
let create_order_totals_table db =
  let drop_sql = "DROP TABLE IF EXISTS order_totals" in
  let create_sql = 
    "CREATE TABLE order_totals (order_id INTEGER PRIMARY KEY, total_amount REAL, total_tax REAL)"
  in
  create_table db "order_totals" drop_sql create_sql

(** Inserts a single order total record into the database.
    @param db SQLite database handle
    @param order_total Order total record to insert
    @return Result indicating success (Ok ()) or failure (Error string) *)
let insert_order_total db order_total =
  let insert_sql = 
    "INSERT INTO order_totals (order_id, total_amount, total_tax) VALUES (?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db insert_sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int order_total.order_id)));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT order_total.total_amount));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT order_total.total_tax));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> 
      ignore (Sqlite3.finalize stmt);
      Ok ()
  | _ -> 
      ignore (Sqlite3.finalize stmt);
      Error ()

(** Writes order totals to a SQLite database.
    @param order_totals List of order total records
    @param db_name SQLite database file path
    @return Result indicating success (Ok ()) or failure (Error string) *)
let write_order_totals_to_sqlite order_totals db_name =
  let db = Sqlite3.db_open db_name in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
    with_transaction db (fun db ->
      match create_order_totals_table db with
      | Error e -> Error e
      | Ok _ ->
          let results = List.map (insert_order_total db) order_totals in
          if List.exists Result.is_error results
          then Error "Some order total insertions failed"
          else Ok ()
    ))

(** Creates the financial_records table in SQLite.
    @param db SQLite database handle
    @return Result indicating success (Ok ()) or failure (Error string) *)
let create_financial_records_table db =
  let drop_sql = "DROP TABLE IF EXISTS financial_records" in
  let create_sql = 
    "CREATE TABLE financial_records (period STRING PRIMARY KEY, revenue REAL, tax REAL)"
  in
  create_table db "financial_records" drop_sql create_sql

(** Inserts a single financial record into the database.
    @param db SQLite database handle
    @param financial_record Financial record to insert
    @return Result indicating success (Ok ()) or failure (Error string) *)
let insert_financial_record db financial_record =
  let insert_sql = 
    "INSERT INTO financial_records (period, revenue, tax) VALUES (?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db insert_sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT financial_record.period));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT financial_record.avg_rev));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT financial_record.avg_tax));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> 
      ignore (Sqlite3.finalize stmt);
      Ok ()
  | _ -> 
      ignore (Sqlite3.finalize stmt);
      Error ()

(** Writes financial records to a SQLite database.
    @param financial_records List of financial records
    @param db_name SQLite database file path
    @return Result indicating success (Ok ()) or failure (Error string) *)
let write_financial_records_to_sqlite financial_records db_name =
  let db = Sqlite3.db_open db_name in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
    with_transaction db (fun db ->
      match create_financial_records_table db with
      | Error e -> Error e
      | Ok _ ->
          let results = List.map (insert_financial_record db) financial_records in
          if List.exists Result.is_error results
          then Error "Some financial record insertions failed"
          else Ok ()))

(** Loads transformed data into CSV and SQLite files.
    @param result Tuple of order_totals and financial_records
    @param filepaths Tuple of base file paths for order totals and financial records
    @return Result indicating success (Ok ()) or failure (Error string) *)
let load result filepaths =
  let (order_totals, financial_records) = result in
  let (ot_filepath, fr_filepath) = filepaths in
  let* _ = write_order_totals_to_csv order_totals (ot_filepath ^ ".csv") in
  let* _ = write_financial_records_to_csv financial_records (fr_filepath ^ ".csv") in
  let* _ = write_order_totals_to_sqlite order_totals (ot_filepath ^ ".sqlite") in
  let* _ = write_financial_records_to_sqlite financial_records (fr_filepath ^ ".sqlite") in
  Ok ()