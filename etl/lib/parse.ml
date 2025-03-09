open Types

let parse_int raw_value = int_of_string raw_value
let parse_float raw_value = float_of_string raw_value

let parse_status = function
  | "Pending" -> Pending
  | "Complete" -> Complete
  | _ -> Cancelled

let parse_origin = function
  | "O" -> O
  | _ -> P

let parse_order_row row =
  let id = Csv.Row.find row "id" |> parse_int in
  let status = Csv.Row.find row "status" |> parse_status in
  let origin = Csv.Row.find row "origin" |> parse_origin in
  { id; status; origin }

let parse_order_item_row row =
  let order_id = Csv.Row.find row "order_id" |> parse_int in
  let quantity = Csv.Row.find row "quantity" |> parse_int in
  let price = Csv.Row.find row "price" |> parse_float in
  let tax = Csv.Row.find row "tax" |> parse_float in
  { order_id; quantity; price; tax }

let parse_orders orders = List.map parse_order_row orders
let parse_order_items order_items = List.map parse_order_item_row order_items