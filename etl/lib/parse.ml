open Types

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Parses a date string into a year-month format.
    @param d Date string to parse
    @return Result containing the first 7 characters (YYYY-MM) if valid, or an error string *)
let parse_date d =
  if String.length d < 7 then
    Error (Printf.sprintf "Date too short: %s (expected at least 7 characters)" d)
  else
    let prefix = String.sub d 0 7 in
    let is_digit c = c >= '0' && c <= '9' in
    let valid_format =
      String.length prefix = 7 &&
      is_digit prefix.[0] && is_digit prefix.[1] &&
      is_digit prefix.[2] && is_digit prefix.[3] &&
      prefix.[4] = '-' &&
      is_digit prefix.[5] && is_digit prefix.[6]
    in
    if valid_format then Ok prefix
    else Error (Printf.sprintf "Invalid date format: %s (expected YYYY-MM-DDTHH:mm:SS)" prefix)

(** Parses a status string into a Status variant.
    @param s Status string to parse
    @return Result containing Status variant (Pending, Complete, Cancelled) or error string *)
let parse_status = function
  | "Pending"   -> Ok Pending
  | "Complete"  -> Ok Complete
  | "Cancelled" -> Ok Cancelled
  | s   -> Error (Printf.sprintf "Unknown status: %s" s)

(** Parses an origin string into an Origin variant.
    @param s Origin string to parse
    @return Result containing Origin variant (O, P) or error string *)
let parse_origin = function
  | "O"       -> Ok O
  | "P"       -> Ok P
  | s -> Error (Printf.sprintf "Unknown origin: %s" s)

(** Generic number parser converting string to a number type.
    @param number_of_string Conversion function (e.g., int_of_string_opt)
    @param field Name of the field being parsed (for error messages)
    @param raw Raw string value to parse
    @return Result containing parsed number or error string *)
let parse_number number_of_string field raw  =
  raw
  |> number_of_string
  |> Option.to_result ~none: (Printf.sprintf "Invalid %s: %s" field raw)

(** Parses a string into an integer.
    @param field Name of the field being parsed
    @param raw Raw string value to parse
    @return Result containing parsed integer or error string *)
let parse_int = parse_number int_of_string_opt

(** Parses a string into a float.
    @param field Name of the field being parsed
    @param raw Raw string value to parse
    @return Result containing parsed float or error string *)
let parse_float = parse_number float_of_string_opt

(** Parses a CSV row into an order record.
    @param row CSV row containing order data
    @return Result containing parsed order record or error string *)
let parse_order row =
  let* id = Csv.Row.find row "id"            |> parse_int "id" in
  let* date = Csv.Row.find row "order_date"  |> parse_date in
  let* status = Csv.Row.find row "status"    |> parse_status in
  let* origin = Csv.Row.find row "origin"    |> parse_origin in
  Ok { id; date; status; origin }

(** Parses a CSV row into an order item record.
    @param row CSV row containing order item data
    @return Result containing parsed order item record or error string *)
let parse_order_item row =
  let* order_id = Csv.Row.find row "order_id"  |> parse_int "order_id" in
  let* quantity = Csv.Row.find row "quantity"  |> parse_int "quantity" in
  let* price = Csv.Row.find row "price"        |> parse_float "price" in
  let* tax = Csv.Row.find row "tax"            |> parse_float "tax" in
  Ok { order_id; quantity; price; tax }

(** Parses a list of items using a given parser function.
    @param parser Function to parse individual items
    @param rows List of items to parse
    @return Result containing list of parsed items or first error encountered *)
let parse_list parser rows =
  let results = List.map parser rows in
  let rec check_results acc = function
    | [] -> Ok (List.rev acc)
    | (Ok x) :: rest -> check_results (x :: acc) rest
    | (Error msg) :: _ -> Error msg
  in
  check_results [] results

(** Parses a list of order items from CSV rows.
    @param order_items List of CSV rows containing order item data
    @return Result containing list of parsed order items or error string *)
let parse_order_item_list order_items =
  parse_list parse_order_item order_items

(** Parses a list of orders from CSV rows.
    @param orders List of CSV rows containing order data
    @return Result containing list of parsed orders or error string *)
let parse_order_list orders =
  parse_list parse_order orders

(** Parses a list of status strings into Status variants.
    @param statuses List of status strings
    @return Result containing list of parsed Status variants or error string *)
let parse_status_list statuses =
  parse_list parse_status statuses

(** Parses a list of origin strings into Origin variants.
    @param origins List of origin strings
    @return Result containing list of parsed Origin variants or error string *)
let parse_origin_list origins =
  parse_list parse_origin origins