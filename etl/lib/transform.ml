open Parse
open Types

(**
    Filters a list of orders by a list of statuses.
    
    @param orders The list of orders to filter.
    @param statuses The list of statuses to filter by.
    @return A new list containing only orders whose status is in the provided statuses list.
*)
let filter_orders_by_statuses orders statuses =
  List.filter (fun order -> List.mem order.status statuses) orders

(**
    Filters a list of orders by a list of origins.
    
    @param orders The list of orders to filter.
    @param origins The list of origins to filter by.
    @return A new list containing only orders whose origin is in the provided origins list.
*)
let filter_orders_by_origins orders origins =
  List.filter (fun order -> List.mem order.origin origins) orders

(**
    Computes the total amount and tax for a specific order based on its items.
    
    @param order_items The list of order items to process.
    @param order_id The ID of the order to compute totals for.
    @return A tuple (total_amount, total_tax) representing the sum of amounts and taxes
            for items matching the given order_id.
*)
let compute_amount_tax_totals (order_items : order_item list) order_id =
  let matching_items = List.filter (fun (item : order_item) -> item.order_id = order_id) order_items in
  List.fold_left
    (fun (amount, tax) item ->
      let item_amount = item.price *. float_of_int item.quantity in
      (amount +. item_amount, tax +. item_amount *. item.tax))
    (0.0, 0.0) matching_items

(**
    Computes total amount and tax for each order based on its items.
    
    @param orders The list of orders to process.
    @param order_items The list of order items to compute totals from.
    @return A list of records containing order_id, total_amount, and total_tax for each order.
*)
let compute_order_totals orders order_items =
  List.map
    (fun order ->
      let total_amount, total_tax = compute_amount_tax_totals order_items order.id in
      { order_id = order.id; total_amount; total_tax })
    orders

(**
    Transforms and filters order data based on provided statuses and origins, then computes order totals.
    
    @param orders The list of orders to process.
    @param order_items The list of order items.
    @param statuses The list of status strings to filter by.
    @param origins The list of origin strings to filter by.
    @return A list of records containing order_id, total_amount, and total_tax for each filtered order.
*)
let transform orders order_items statuses origins =
  let parsed_statuses = List.map parse_status statuses in
  let parsed_origins = List.map parse_origin origins in
  let filtered_orders = filter_orders_by_statuses orders parsed_statuses in
  let filtered_orders = filter_orders_by_origins filtered_orders parsed_origins in
  let order_totals = compute_order_totals filtered_orders order_items in
  order_totals
