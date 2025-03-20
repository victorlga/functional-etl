open Types

let parse_date d = String.sub d 0 7

(**
    Parses a status string into a status variant.
    
    @param s The status string to parse ("Pending", "Complete", or "Cancelled").
    @return The corresponding status variant.
    @raise Failure If the status string is invalid.
*)
let parse_status = function
  | "Pending"   -> Pending
  | "Complete"  -> Complete
  | "Cancelled" -> Cancelled
  | s   -> failwith (Printf.sprintf "Invalid status: %s" s)

(**
    Parses an origin string into an origin variant.
    
    @param s The origin string to parse ("O" or "P").
    @return The corresponding origin variant.
    @raise Failure If the origin string is invalid.
*)
let parse_origin = function
  | "O"       -> O
  | "P"       -> P
  | s -> failwith (Printf.sprintf "Invalid origin: %s" s)

(**
    Parses a CSV row into an order record.
    
    @param row The CSV row containing order data.
    @return A record containing the parsed order data.
    @raise Failure If any field cannot be parsed correctly.
*)
let parse_order row=
  let id = Csv.Row.find row "id"            |> int_of_string in
  let date = Csv.Row.find row "order_date"  |> parse_date in
  let status = Csv.Row.find row "status"    |> parse_status in
  let origin = Csv.Row.find row "origin"    |> parse_origin in
  { id; date; status; origin }

(**
    Parses a CSV row into an order item record.
    
    @param row The CSV row containing order item data.
    @return A record containing the parsed order item data.
    @raise Failure If any field cannot be parsed correctly.
*)
let parse_order_item row =
  let order_id = Csv.Row.find row "order_id"  |> int_of_string in
  let quantity = Csv.Row.find row "quantity"  |> int_of_string in
  let price = Csv.Row.find row "price"        |> float_of_string in
  let tax = Csv.Row.find row "tax"            |> float_of_string in
  { order_id; quantity; price; tax }
