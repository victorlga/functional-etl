open Lib.Extract
open Lib.Parse
open Lib.Transform
open Lib.Load

(** MADE WITH GROK
    Prompts the user to input a list of strings, one per line, until an empty line is entered.
    @param prompt The prompt to display to the user before each input
    @return A list of strings entered by the user, in the order they were provided
    @raise End_of_file if the input stream is closed unexpectedly *)
let get_list_from_user prompt =
  print_string prompt;
  flush stdout;
  let rec loop acc =
    let input = input_line stdin in
    if input = "" then List.rev acc
    else loop (input :: acc)
  in
  loop []

(** Main function to process order data: extracts, parses, filters, transforms, and writes order totals.
    @param orders_filepath Path to the CSV file containing order data
    @param order_item_filepath Path to the CSV file containing order item data
    @param order_totals_filepath Path to write the resulting order totals CSV
    @param statuses List of status strings to filter orders by
    @param origins List of origin strings to filter orders by
    @raise Sys_error if any file operation fails
    @raise Failure if parsing of statuses, origins, or CSV data fails *)
let main orders_filepath order_item_filepath order_totals_filepath statuses origins =
  let raw_orders = extract_csv_from_file orders_filepath in
  let parsed_orders = List.map parse_order raw_orders in
  let parsed_statuses = List.map parse_status statuses in
  let parsed_origins = List.map parse_origin origins in
  let orders_filtered_by_status = filter_orders_by_statuses parsed_orders parsed_statuses in
  let orders = filter_orders_by_origins orders_filtered_by_status parsed_origins in
  let raw_order_items = extract_csv_from_file order_item_filepath in
  let order_items = List.map parse_order_item raw_order_items in
  let order_totals = compute_order_totals orders order_items in
  write_order_totals_to_csv order_totals_filepath order_totals

(** Entry point of the program. Prompts the user for statuses and origins, then processes order data.
    @raise End_of_file if the input stream is closed unexpectedly
    @raise Sys_error if file operations fail
    @raise Failure if parsing fails *)
let () =
  print_endline "Please enter the statuses you want to consider (e.g., Pending, Cancelled, Complete). Enter one per line, and press Enter twice to finish:";
  let statuses = get_list_from_user "Status: " in
  print_endline "Please enter the origins you want to consider (e.g., O, P). Enter one per line, and press Enter twice to finish:";
  let origins = get_list_from_user "Origin: " in
  main "data/order.csv" "data/order_item.csv" "data/order_totals.csv" statuses origins