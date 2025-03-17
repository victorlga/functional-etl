open Types

(**
    Writes order total records to a CSV file.
    
    @param filepath The path to the output CSV file.
    @param records A list of records containing order_id, total_amount, and total_tax.
    @raise Sys_error If the file cannot be opened or written to.
    @raise Failure If an error occurs during writing, after ensuring the file is properly closed.
    @author Adapted from https://grok.com generated code.
*)
let write_order_totals_to_csv order_totals filepath =
  let header = "order_id,total_amount,total_tax\n" in
  let oc = open_out filepath in
  try
    output_string oc header;
    List.iter
      (fun order_total ->
        let row =
          String.concat ","
            [
              string_of_int order_total.order_id;
              string_of_float order_total.total_amount;
              string_of_float order_total.total_tax;
            ]
        in
        output_string oc (row ^ "\n"))
      order_totals;
    close_out oc
  with e ->
    close_out oc;
    raise e


(**
    Creates or drops and recreates the order_totals table in the SQLite database.
    
    @param db The SQLite database connection.
    @return Unit (side effect: creates table or prints error).
    @raise Sqlite3.Error If the table creation query fails unexpectedly.
*)
let create_order_totals_table db =
  let drop_table_sql = "DROP TABLE IF EXISTS order_totals" in
  ignore(Sqlite3.exec db drop_table_sql);

  let create_table_sql = 
    "CREATE TABLE order_totals (order_id INTEGER PRIMARY KEY, total_amount REAL, total_tax REAL)" 
  in
  match Sqlite3.exec db create_table_sql with
  | Sqlite3.Rc.OK -> print_endline "Table created successfully"
  | _ -> print_endline "Failed to create table"


(**
    Inserts a single order total into the order_totals table.
    
    @param db The SQLite database connection.
    @param order_total The order total record to insert, containing order_id, total_amount, and total_tax.
    @return Unit (side effect: inserts data or raises an exception).
    @raise Failure If the insert operation fails.
    @raise Sqlite3.Error If SQLite binding or stepping fails unexpectedly.
*)
let insert_order_total db order_total =
  let insert_sql = 
    "INSERT INTO order_totals (order_id, total_amount, total_tax) VALUES (?, ?, ?)" 
  in
  Printf.printf "Inserting order_id: %d\n" order_total.order_id;
  let stmt = Sqlite3.prepare db insert_sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int order_total.order_id)));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT order_total.total_amount));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT order_total.total_tax));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ignore (Sqlite3.finalize stmt); print_endline "Insert successful"
  | rc -> 
      ignore (Sqlite3.finalize stmt); 
      Printf.printf "Insert failed with code %s: %s\n" 
        (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db);
      failwith "Insert failed"


(**
    Writes a list of order totals to a SQLite database.
    
    @param order_totals List of order total records to write.
    @param db_name The name of the SQLite database file.
    @return Unit (side effect: creates table and inserts data).
    @raise Failure If any insert operation fails.
    @raise Sqlite3.Error If database operations fail unexpectedly.
*)
let write_order_totals_to_sqlite order_totals db_name =
  let db = Sqlite3.db_open db_name in
  create_order_totals_table db;
  Printf.printf "Processing %d order_totals\n" (List.length order_totals);
  List.iter (insert_order_total db) order_totals;
  ignore (Sqlite3.db_close db)


(**
    Saves order total records to a CSV file.
    
    @param order_totals A list of order total records.
    @param filepath The path to the output CSV file.
    @raise Sys_error If file operations fail.
    @raise Failure If an error occurs during writing.
*)
let load order_totals filepath =
  write_order_totals_to_csv order_totals (filepath ^ ".csv") ;
  write_order_totals_to_sqlite order_totals (filepath ^ ".sqlite")