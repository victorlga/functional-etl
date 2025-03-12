open Lib.Extract
open Lib.Transform
open Lib.Load

(**
    Prompts the user to input a list of strings, one per line, until an empty line is entered.
    
    @param prompt A string displayed to the user before each input prompt.
    @return A list of user-provided strings, in the order they were entered.
    @raise End_of_file If the input stream is closed unexpectedly.
*)
let get_list_from_user prompt =
  print_string prompt;
  flush stdout;
  let rec loop acc =
    let input = input_line stdin in
    if input = "" then List.rev acc
    else loop (input :: acc)
  in
  loop []

(**
    Processes order data by extracting, transforming, and writing order totals.
    
    @param orders_filepath Path to the CSV file containing order data.
    @param order_item_filepath Path to the CSV file containing order item data.
    @param order_totals_filepath Path to save the computed order totals CSV.
    @param statuses List of order status strings to filter.
    @param origins List of order origin strings to filter.
    @raise Sys_error If any file operation fails.
    @raise Failure If parsing of statuses, origins, or CSV data fails.
*)
let main orders_filepath order_item_filepath order_totals_filepath statuses origins =
  let (orders, order_items) = extract orders_filepath order_item_filepath in
  let order_totals = transform orders order_items statuses origins in
  load order_totals order_totals_filepath

(**
    Entry point of the program. Prompts the user for statuses and origins, then processes order data.
    
    @raise End_of_file If the input stream is closed unexpectedly.
    @raise Sys_error If file operations fail.
    @raise Failure If parsing fails.
*)
let () =
  print_endline "Please enter the statuses you want to consider (e.g., Pending, Cancelled, Complete). Enter one per line, and press Enter twice to finish:";
  let statuses = get_list_from_user "Status: " in
  print_endline "Please enter the origins you want to consider (e.g., O, P). Enter one per line, and press Enter twice to finish:";
  let origins = get_list_from_user "Origin: " in
  main "data/order.csv" "data/order_item.csv" "data/order_totals.csv" statuses origins
