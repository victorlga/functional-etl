open Types

let ( let* ) = Result.bind

let parse_date d = Ok (String.sub d 0 7)


let parse_status = function
  | "Pending"   -> Ok Pending
  | "Complete"  -> Ok Complete
  | "Cancelled" -> Ok Cancelled
  | s   -> Error (Printf.sprintf "Unknown status: %s" s)


let parse_origin = function
  | "O"       -> Ok O
  | "P"       -> Ok P
  | s -> Error (Printf.sprintf "Unknown origin: %s" s)


let parse_number number_of_string field raw  =
  raw
  |> number_of_string
  |> Option.to_result ~none: (Printf.sprintf "Invalid %s: %s" field raw)


let parse_int = parse_number int_of_string_opt

let parse_float = parse_number float_of_string_opt


let parse_order row=
  let* id = Csv.Row.find row "id"            |> parse_int "id" in
  let* date = Csv.Row.find row "order_date"  |> parse_date in
  let* status = Csv.Row.find row "status"    |> parse_status in
  let* origin = Csv.Row.find row "origin"    |> parse_origin in
  Ok { id; date; status; origin }


let parse_order_item row =
  let* order_id = Csv.Row.find row "order_id"  |> parse_int "order_id" in
  let* quantity = Csv.Row.find row "quantity"  |> parse_int "quantity" in
  let* price = Csv.Row.find row "price"        |> parse_float "price" in
  let* tax = Csv.Row.find row "tax"            |> parse_float "tax" in
  Ok { order_id; quantity; price; tax }


let parse_list parser rows =
  let results = List.map parser rows in
  let rec check_results acc = function
    | [] -> Ok (List.rev acc)
    | (Ok x) :: rest -> check_results (x :: acc) rest
    | (Error msg) :: _ -> Error msg
  in
  check_results [] results

let parse_order_item_list order_items =
  parse_list parse_order_item order_items

let parse_order_list orders =
  parse_list parse_order orders

let parse_status_list statuses =
  parse_list parse_status statuses

let parse_origin_list origins =
  parse_list parse_origin origins