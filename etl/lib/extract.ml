let extract_csv_from_file filepath =
  Csv.Rows.load ~has_header:true filepath