open Lib.Extract
open Lib.Parse
open Lib.Transform
open Lib.Load

let get_list_from_user prompt =
  (* Made with Grok *)
  print_string prompt;
  flush stdout;
  let rec loop acc =
    let input = input_line stdin in
    if input = "" then List.rev acc
    else loop (input :: acc)
  in
  loop []

let main orders_filepath order_item_filepath order_totals_filepath statuses origins =
  let raw_orders = extract_csv_from_file orders_filepath in
  let parsed_orders = parse_orders raw_orders in
  let parsed_statuses = List.map parse_status statuses in
  let parsed_origins = List.map parse_origin origins in
  let orders_filtered_by_status = filter_orders_by_statuses parsed_orders parsed_statuses in
  let orders = filter_orders_by_origins orders_filtered_by_status parsed_origins in
  let raw_order_items = extract_csv_from_file order_item_filepath in
  let order_items = parse_order_items raw_order_items in
  let order_totals = compute_order_totals orders order_items in
  write_to_csv order_totals_filepath order_totals "order_id,total_amount,total_tax"

let () =
  print_endline "Please enter the statuses you want to consider (e.g., Pending, Cancelled, Complete). Enter one per line, and press Enter twice to finish:";
  let statuses = get_list_from_user "Status: " in
  print_endline "Please enter the origins you want to consider (e.g., O, P). Enter one per line, and press Enter twice to finish:";
  let origins = get_list_from_user "Origin: " in
  main "data/order.csv" "data/order_item.csv" "data/order_totals.csv" statuses origins