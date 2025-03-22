open Lib.Extract
open Lib.Transform
open Lib.Load

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Reads a list of strings from user input until an empty line is entered.
    @param prompt The prompt to display to the user before input
    @return A list of strings entered by the user in reverse order of input *)
let get_list_from_user prompt =
  print_string prompt;
  flush stdout;
  let rec loop acc =
    let input = input_line stdin in
    if input = "" then List.rev acc
    else loop (input :: acc)
  in
  loop []

(** Main processing pipeline that extracts, transforms, and loads data.
    @param data_filepaths Tuple of file paths for input data (orders, order_items)
    @param result_filepaths Tuple of file paths for output results (order_totals, financial_records)
    @param criterias Tuple of statuses and origins to filter the data
    @return Result type indicating success (Ok ()) or failure (Error string) *)
let main data_filepaths result_filepaths criterias =
  let* data = extract data_filepaths in
  let* result = transform data criterias in
  load result result_filepaths

(** Entry point of the program *)
let () =
  (** Input data file paths *)
  let data_filepaths = ("data/orders.csv", "data/order_items.csv") in
  (** Output result file paths *)
  let result_filepaths = ("data/ot/order_totals", "data/fr/financial_records") in

  print_endline "Please enter the statuses you want to consider (e.g., Pending, Cancelled, Complete). Entry one per line, and press Enter twice to finish:";
  (** List of status filters *)
  let statuses = get_list_from_user "Status: " in

  print_endline "Please enter the origins you want to consider (e.g., O, P). Enter one per line, and press Enter twice to finish:";
  (** List of origin filters *)
  let origins = get_list_from_user "Origin: " in

  (** Combined criteria for data processing *)
  let criterias = (statuses, origins) in

  match main data_filepaths result_filepaths criterias with
  | Error str -> failwith (str)
  | Ok () -> print_endline "SUCESS"