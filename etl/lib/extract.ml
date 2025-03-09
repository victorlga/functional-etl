(** Loads CSV data from a file into a list of rows.
    @param filepath The path to the CSV file to load
    @return A list of CSV rows, where each row is a record of column values
    @raise Sys_error if the file cannot be opened or read
    @raise Csv.Error if the CSV format is invalid *)
let extract_csv_from_file filepath =
  Csv.Rows.load ~has_header:true filepath