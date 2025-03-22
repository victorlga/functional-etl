open Lib.Extract
open Lib.Transform
open Lib.Load

let ( let* ) = Result.bind


let get_list_from_user prompt =
  print_string prompt;
  flush stdout;
  let rec loop acc =
    let input = input_line stdin in
    if input = "" then List.rev acc
    else loop (input :: acc)
  in
  loop []


let main data_filepaths result_filepaths criterias =
  let* data = extract data_filepaths in
  let* result = transform data criterias in
  load result result_filepaths

let () =
  let data_filepaths = ("data/orders.csv", "data/order_items.csv") in
  let result_filepaths = ("data/ot/order_totals", "data/fr/financial_records") in

  print_endline "Please enter the statuses you want to consider (e.g., Pending, Cancelled, Complete). Entry one per line, and press Enter twice to finish:";
  let statuses = get_list_from_user "Status: " in

  print_endline "Please enter the origins you want to consider (e.g., O, P). Enter one per line, and press Enter twice to finish:";
  let origins = get_list_from_user "Origin: " in

  let criterias = (statuses, origins) in
  match main data_filepaths result_filepaths criterias with
  | Error str -> failwith (str)
  | Ok () -> print_endline "SUCESS"