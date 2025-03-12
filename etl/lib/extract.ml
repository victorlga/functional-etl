open Parse

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
    Extracts order and order item data from CSV files.
    
    @param orders_filepath Path to the CSV file containing order data.
    @param order_item_filepath Path to the CSV file containing order item data.
    @return A tuple containing lists of parsed orders and order items.
    @raise Sys_error If file operations fail.
    @raise Csv.Error If the CSV format is invalid.
    @raise Failure If parsing of order or order item data fails.
*)
let extract orders_filepath order_item_filepath =
  let raw_orders = extract_csv_from_file orders_filepath in
  let orders = List.map parse_order raw_orders in
  let raw_order_items = extract_csv_from_file order_item_filepath in
  let order_items = List.map parse_order_item raw_order_items in
  (orders, order_items)
