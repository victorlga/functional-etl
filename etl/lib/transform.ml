open Parse
open Types

(**
    Filters a list of orders by a list of statuses.
    
    @param orders The list of orders to filter.
    @param statuses The list of statuses to filter by.
    @return A new list containing only orders whose status is in the provided statuses list.
*)
let filter_orders_by_statuses orders statuses =
  List.filter (fun (order: order) -> List.mem order.status statuses) orders


(**
    Filters a list of orders by a list of origins.
    
    @param orders The list of orders to filter.
    @param origins The list of origins to filter by.
    @return A new list containing only orders whose origin is in the provided origins list.
*)
let filter_orders_by_origins orders origins =
  List.filter (fun (order: order) -> List.mem order.origin origins) orders


(**
    Performs an inner join between orders and order items based on order ID.
    
    @param orders A list of order records, each containing id, status, and origin.
    @param order_items A list of order item records, each containing order_id, quantity, price, and tax.
    @return A list of combined records, each containing fields from both order and order_item where order.id matches order_item.order_id.
*)
let inner_join_orders_items orders order_items=
  List.concat_map (fun order ->
    List.filter_map (fun (item: order_item) ->
      if order.id = item.order_id then
        Some {
          order_id = order.id;
          date = order.date;
          status = order.status;
          origin = order.origin;
          quantity = item.quantity;
          price = item.price;
          tax = item.tax
        }
      else
        None
    ) order_items
  ) orders


(**
    Calculates the amount for a single order item based on its price and quantity.
    
    @param o_w_i The order item record containing price and quantity.
    @return The computed amount (price * quantity) as a float.
*)
let compute_amount o_w_i =
  o_w_i.price *. float_of_int o_w_i.quantity


(**
    Updates the running totals of amount and tax for an order in an immutable map.
    
    @param aux The map storing order totals, mapping order IDs to (total_amount, total_tax) tuples.
    @param o_w_i The order item record containing order_id, price, quantity, and tax.
    @return A new map with the updated totals, as maps are immutable.
*)
let update_order_totals aux (o_w_i : order_with_item) =
  let prev_values = OrderTotalMap.find_opt o_w_i.order_id aux |> Option.value ~default:(0.0, 0.0) in
  let prev_amount, prev_tax = prev_values in
  let amount = compute_amount o_w_i in
  let total_amount = prev_amount +. amount in
  let total_tax = prev_tax +. amount *. o_w_i.tax in
  OrderTotalMap.add o_w_i.order_id (total_amount, total_tax) aux


(**
    Converts a map of order totals into a list of order_total records.
    
    @param map The map storing order totals, mapping order IDs to (total_amount, total_tax) tuples.
    @return A list of order_total records, each containing order_id, total_amount, and total_tax.
*)
let order_total_map_to_list map =
  OrderTotalMap.fold (fun order_id (total_amount, total_tax) acc ->
    { order_id; total_amount; total_tax } :: acc
  ) map []


(**
    Computes total amount and tax for each order based on its items.
    
    @param orders_with_items The list of order items to compute totals from.
    @return A list of order_total records, each containing order_id, total_amount, and total_tax.
    
    This function processes the list of order items using a fold operation with an immutable map,
    accumulating order totals and then converting them into a list.
*)
let compute_order_totals orders_with_items =
  List.fold_left (fun aux o_w_i ->
    update_order_totals aux o_w_i
  ) OrderTotalMap.empty orders_with_items 
  |> order_total_map_to_list


(* let compute_financial_records orders_with_items = ("") *)
  
let parse_and_filter_orders raw_orders criterias = 
  let (statuses, origins) = criterias in
  let orders = List.map parse_order raw_orders in
  let parsed_statuses = List.map parse_status statuses in
  let parsed_origins = List.map parse_origin origins in
  let filtered_orders = filter_orders_by_statuses orders parsed_statuses in
  let filtered_orders = filter_orders_by_origins filtered_orders parsed_origins in
  filtered_orders

let parse_order_items order_items =
  List.map parse_order_item order_items

(**
    Transforms and filters order data based on provided statuses and origins, then computes order totals.
    
    @param orders The list of orders to process.
    @param order_items The list of order items.
    @param statuses The list of status strings to filter by.
    @param origins The list of origin strings to filter by.
    @return A list of records containing order_id, total_amount, and total_tax for each filtered order.
*)
let transform data criterias =
  let (raw_orders, raw_order_items) = data in
  let filtered_orders = parse_and_filter_orders raw_orders criterias in
  let order_items = parse_order_items raw_order_items in
  let orders_with_items = inner_join_orders_items filtered_orders order_items in
  let order_totals = compute_order_totals orders_with_items in
  (* let financial_records = compute_financial_records orders_with_items in *)
  (order_totals, "")

