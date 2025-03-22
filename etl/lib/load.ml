open Types

let ( let* ) = Result.bind

(* Feito com o GROK *)
let with_output_channel filepath f =
  let oc = open_out filepath in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)


let write_csv header to_row items filepath =
  with_output_channel filepath (fun oc ->
    output_string oc header;
    List.iter (fun item -> output_string oc (to_row item ^ "\n")) items)


let write_order_totals_to_csv order_totals filepath =
  let header = "order_id,total_amount,total_tax\n" in
  let to_row ot = 
    Printf.sprintf "%d,%.2f,%.2f"
      ot.order_id
      ot.total_amount
      ot.total_tax
  in
  write_csv header to_row order_totals filepath;
  Ok "Order totals CSV written successfully"


let write_financial_records_to_csv financial_records filepath =
  let header = "order_id,total_amount,total_tax\n" in
  let to_row fr = 
    Printf.sprintf "%s,%.2f,%.2f"
      fr.period
      fr.revenue
      fr.tax
  in
  write_csv header to_row financial_records filepath;
  Ok "Financial records CSV written successfully"


let create_table db table_name drop_sql create_sql =
  ignore (Sqlite3.exec db drop_sql);
  match Sqlite3.exec db create_sql with
  | Sqlite3.Rc.OK -> Ok (Printf.sprintf "Table %s created" table_name)
  | rc -> Error (Printf.sprintf "Failed to create %s: %s" 
                  table_name (Sqlite3.Rc.to_string rc))


(* Feito com o GROK *)
let with_transaction db f =
  Sqlite3.exec db "BEGIN TRANSACTION" |> ignore;
  try
    let result = f db in
    Sqlite3.exec db "COMMIT" |> ignore;
    Ok result
  with e ->
    Sqlite3.exec db "ROLLBACK" |> ignore;
    Error (Printexc.to_string e)


let create_order_totals_table db =
  let drop_sql = "DROP TABLE IF EXISTS order_totals" in
  let create_sql = 
    "CREATE TABLE order_totals (order_id INTEGER PRIMARY KEY, total_amount REAL, total_tax REAL)"
  in
  create_table db "order_totals" drop_sql create_sql


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
  | rc -> 
      ignore (Sqlite3.finalize stmt);
      Error (Printf.sprintf "Insert failed with code %s: %s"
              (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))


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
          else Ok "Order totals inserted successfully"
    ))


let create_financial_records_table db =
  let drop_sql = "DROP TABLE IF EXISTS financial_records" in
  let create_sql = 
    "CREATE TABLE financial_records (period STRING PRIMARY KEY, revenue REAL, tax REAL)"
  in
  create_table db "financial_records" drop_sql create_sql


let insert_financial_record db financial_record =
  let insert_sql = 
    "INSERT INTO financial_records (period, revenue, tax) VALUES (?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db insert_sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT financial_record.period));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT financial_record.revenue));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT financial_record.tax));

  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> 
      ignore (Sqlite3.finalize stmt);
      Ok ()
  | rc -> 
      ignore (Sqlite3.finalize stmt);
      Error (Printf.sprintf "Insert failed with code %s: %s"
              (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))


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
          else Ok "Financial records inserted successfully"))


let load result filepaths =
  let (order_totals, financial_records) = result in
  let (ot_filepath, fr_filepath) = filepaths in
  let* _ = write_order_totals_to_csv order_totals (ot_filepath ^ ".csv") in
  let* _ = write_financial_records_to_csv financial_records (fr_filepath ^ ".csv") in
  let* _ = write_order_totals_to_sqlite order_totals (ot_filepath ^ ".sqlite") in
  let* _ = write_financial_records_to_sqlite financial_records (fr_filepath ^ ".sqlite") in
  Ok ()
