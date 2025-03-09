open Types

(** MADE WITH GROK
    Writes order total records to a CSV file.
    @param filepath The path to the output CSV file
    @param records A list of records containing order_id, total_amount, and total_tax
    @raise Sys_error if the file cannot be opened or written to
    @raise e Any exception that occurs during writing will be re-raised after cleanup *)
let write_order_totals_to_csv filepath records =
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