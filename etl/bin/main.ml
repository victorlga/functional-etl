open Lib.Extract
open Lib.Parse
open Lib.Transform
open Lib.Load

let main orders_filepath order_item_filepath order_totals_filepath status origin =
  let raw_orders = extract_csv_from_file orders_filepath in
  let parsed_orders = parse_orders raw_orders in
  let parsed_status = parse_status status in
  let parsed_origin = parse_origin origin in
  let orders_filtered_by_status = filter_orders_by_status parsed_orders parsed_status in
  let orders = filter_orders_by_origin orders_filtered_by_status parsed_origin in
  let raw_order_items = extract_csv_from_file order_item_filepath in
  let order_items = parse_order_items raw_order_items in
  let order_totals = compute_order_totals orders order_items in
  write_to_csv order_totals_filepath order_totals "order_id,total_amount,total_tax"

let () = main "data/order.csv" "data/order_item.csv" "data/order_totals.csv" "Pending" "O"