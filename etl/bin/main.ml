
(* Início do Extract *)

let extract_csv_from_file filepath = Csv.Rows.load ~has_header:true filepath

let extract_csv_from_web url = url

(* Início do Parse *)

type status = Pending | Complete | Cancelled
type origin = O | P

type order = {
  id : int;
  client_id : int;
  order_datetime : string;
  status : status;
  origin : origin;
}

type order_item = {
  order_id: int;
  product_id: int;
  quantity: int;
  price: float;
  tax: float;
}



(* Generic parse_int and parse_float function with a custom error constructor *)
let parse_int raw_value = int_of_string raw_value

let parse_float raw_value = float_of_string raw_value

(* Other parsing functions *)
let parse_status = function
  | "Pending" -> Pending
  | "Complete" -> Complete
  | _ -> Cancelled

let parse_origin = function
  | "O" -> O
  | _ -> P

(* Parsing order row *)
let parse_order_row row =
  let id = Csv.Row.find row "id" |> parse_int in
  let client_id = Csv.Row.find row "client_id" |> parse_int in
  let order_datetime = Csv.Row.find row "order_date" in
  let status = Csv.Row.find row "status" |> parse_status in
  let origin = Csv.Row.find row "origin" |> parse_origin in
  { id; client_id; order_datetime; status; origin }

(* Parsing order item row *)
let parse_order_item_row row =
  let order_id = Csv.Row.find row "order_id" |> parse_int in
  let product_id = Csv.Row.find row "product_id" |> parse_int in
  let quantity = Csv.Row.find row "quantity" |> parse_int in
  let price = Csv.Row.find row "price" |> parse_float in
  let tax = Csv.Row.find row "tax" |> parse_float in
  { order_id; product_id; quantity; price; tax }

let parse_orders orders = List.map parse_order_row orders
let parse_order_items order_items = List.map parse_order_item_row order_items

(* Início do Transform *)

let filter_orders_by_status orders status =
  List.filter (fun order -> order.status = status) orders

let filter_orders_by_origin orders origin =
  List.filter (fun order -> order.origin = origin) orders

type order_totals = {
  order_id: int;
  total_amount: float;
  total_tax: float;
}

let compute_total_amount (order_items: order_item list) order_id =
  let matching_items = List.filter (fun (item: order_item) -> item.order_id = order_id) order_items in
  List.fold_left (
    fun acc item ->
      acc +. item.price *. (float_of_int item.quantity)
  ) 0.0 matching_items

let compute_total_tax (order_items: order_item list) order_id =
  let matching_items = List.filter (fun (item: order_item) -> item.order_id = order_id) order_items in
  List.fold_left (
    fun acc item ->
      acc +. item.price *. item.tax
  ) 0.0 matching_items

let compute_order_totals orders order_items =
  List.map (
    fun order -> {
      order_id = order.id;
      total_amount = (compute_total_amount order_items order.id);
      total_tax =(compute_total_tax order_items order.id)
    }
  ) orders

let load_to_csv order_totals order_totals_filepath =
  ()

(* Toda a lógica reunida *)

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
  load_to_csv order_totals order_totals_filepath


let () = main "data/order.csv" "data/order_item.csv" "data/order_totals.csv" "Pending" "O"
