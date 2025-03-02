let (let*) = Result.bind

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

(* Define a custom error type *)
type error =
  | Unknown_status of string
  | Unknown_origin of string
  | Invalid_id of string
  | Invalid_client_id of string
  | Invalid_order_id of string
  | Invalid_product_id of string
  | Invalid_quantity of string
  | Invalid_price of string
  | Invalid_tax of string

(* Generic parse_to_int and parse_to_float function with a custom error constructor *)
let parse_to_int ~error raw_value =
  raw_value
  |> int_of_string_opt
  |> Option.to_result ~none:(error raw_value)

let parse_to_float ~error raw_value =
  raw_value
  |> float_of_string_opt
  |> Option.to_result ~none:(error raw_value)

(* Specific parsing functions using parse_to_int *)
let parse_id = parse_to_int ~error:(fun raw -> Invalid_id raw)
let parse_client_id = parse_to_int ~error:(fun raw -> Invalid_client_id raw)
let parse_order_id = parse_to_int ~error:(fun raw -> Invalid_order_id raw)
let parse_product_id = parse_to_int ~error:(fun raw -> Invalid_product_id raw)
let parse_quantity = parse_to_int ~error:(fun raw -> Invalid_quantity raw)

(* Specific parsing functions using parse_to_float *)
let parse_price = parse_to_float ~error:(fun raw -> Invalid_price raw)
let parse_tax = parse_to_float ~error:(fun raw -> Invalid_tax raw)

(* Other parsing functions *)
let parse_status = function
  | "Pending" -> Ok Pending
  | "Complete" -> Ok Complete
  | "Cancelled" -> Ok Cancelled
  | s -> Error (Unknown_status s)

let parse_origin = function
  | "O" -> Ok O
  | "P" -> Ok P
  | s -> Error (Unknown_origin s)

(* Parsing order row *)
let parse_order_row row =
  let* id = Csv.Row.find row "id" |> parse_id in
  let* client_id = Csv.Row.find row "client_id" |> parse_client_id in
  let order_datetime = Csv.Row.find row "order_date" in
  let* status = Csv.Row.find row "status" |> parse_status in
  let* origin = Csv.Row.find row "origin" |> parse_origin in
  Ok { id; client_id; order_datetime; status; origin }

let parse_orders orders = List.map parse_order_row orders

(* Parsing order item row *)
let parse_order_item_row row =
  let* order_id = Csv.Row.find row "order_id" |> parse_order_id in
  let* product_id = Csv.Row.find row "product_id" |> parse_product_id in
  let* quantity = Csv.Row.find row "quantity" |> parse_quantity in
  let* price = Csv.Row.find row "price" |> parse_price in
  let* tax = Csv.Row.find row "tax" |> parse_tax in
  Ok { order_id; product_id; quantity; price; tax }

let parse_order_items order_items = List.map parse_order_item_row order_items

let extract_from_csv filename = Csv.Rows.load ~has_header:true filename

(* Add these string conversion/helper functions near your type definitions *)
let string_of_status = function
  | Pending -> "Pending"
  | Complete -> "Complete"
  | Cancelled -> "Cancelled"

let string_of_origin = function
  | O -> "O"
  | P -> "P"

let string_of_order o =
  Printf.sprintf "{id=%d; client_id=%d; order_datetime=%s; status=%s; origin=%s}"
    o.id o.client_id o.order_datetime (string_of_status o.status) (string_of_origin o.origin)

let string_of_order_item oi =
  Printf.sprintf "{order_id=%d; product_id=%d; quantity=%d; price=%.2f; tax=%.2f}"
    oi.order_id oi.product_id oi.quantity oi.price oi.tax

let string_of_error = function
  | Unknown_status s -> Printf.sprintf "Unknown_status: %s" s
  | Unknown_origin s -> Printf.sprintf "Unknown_origin: %s" s
  | Invalid_id s -> Printf.sprintf "Invalid_id: %s" s
  | Invalid_client_id s -> Printf.sprintf "Invalid_client_id: %s" s
  | Invalid_order_id s -> Printf.sprintf "Invalid_order_id: %s" s
  | Invalid_product_id s -> Printf.sprintf "Invalid_product_id: %s" s
  | Invalid_quantity s -> Printf.sprintf "Invalid_quantity: %s" s
  | Invalid_price s -> Printf.sprintf "Invalid_price: %s" s
  | Invalid_tax s -> Printf.sprintf "Invalid_tax: %s" s

(* Modify the main function *)
let main orders_filename order_item_filename =
  let raw_orders = extract_from_csv orders_filename in
  let parsed_orders = parse_orders raw_orders in
  let raw_order_item = extract_from_csv order_item_filename in
  let parsed_order_item = parse_order_items raw_order_item in
  
  (* Print orders *)
  print_endline "Parsed Orders:";
  List.iter (fun order_result ->
    match order_result with
    | Ok order -> print_endline (string_of_order order)
    | Error err -> print_endline ("Error: " ^ string_of_error err)
  ) parsed_orders;
  
  (* Print order items *)
  print_endline "\nParsed Order Items:";
  List.iter (fun item_result ->
    match item_result with
    | Ok item -> print_endline (string_of_order_item item)
    | Error err -> print_endline ("Error: " ^ string_of_error err)
  ) parsed_order_item

let () = main "data/order.csv" "data/order_item.csv"
