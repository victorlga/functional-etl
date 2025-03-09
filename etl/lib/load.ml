open Types

let write_to_csv filepath records header =
  let oc = open_out filepath in
  try
    output_string oc (header ^ "\n");
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