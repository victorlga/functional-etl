open Types

(**
    Writes order total records to a CSV file.
    
    @param filepath The path to the output CSV file.
    @param records A list of records containing order_id, total_amount, and total_tax.
    @raise Sys_error If the file cannot be opened or written to.
    @raise Failure If an error occurs during writing, after ensuring the file is properly closed.
*)
let write_order_totals_to_csv records filepath =
  let header = "order_id,total_amount,total_tax\n" in
  let oc = open_out filepath in
  try
    output_string oc header;
    List.iter
      (fun record ->
        let row =
          String.concat ","
            [
              string_of_int record.order_id;
              string_of_float record.total_amount;
              string_of_float record.total_tax;
            ]
        in
        output_string oc (row ^ "\n"))
      records;
    close_out oc
  with e ->
    close_out oc;
    raise e

(**
    Saves order total records to a CSV file.
    
    @param order_totals A list of order total records.
    @param filepath The path to the output CSV file.
    @raise Sys_error If file operations fail.
    @raise Failure If an error occurs during writing.
*)
let load order_totals filepath =
  write_order_totals_to_csv order_totals filepath
