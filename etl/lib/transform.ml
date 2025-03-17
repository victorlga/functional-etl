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
let inner_join_orders_items orders order_items =
  List.concat_map (fun order ->
    List.filter_map (fun (item: order_item) ->
      if order.id = item.order_id then
        Some {
          order_id = order.id;
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
let compute_amount (o_w_i : order_with_item) : float =
  o_w_i.price *. float_of_int o_w_i.quantity


(**
    Updates the running totals of amount and tax for an order in a hashtable.
    
    @param aux The hashtable storing order totals, mapping order IDs to (total_amount, total_tax) tuples.
    @param o_w_i The order item record containing order_id, price, quantity, and tax.
    @return Unit, as the hashtable is modified in place.
*)
let update_order_totals (aux : (int, float * float) Hashtbl.t) (o_w_i : order_with_item) : unit =
  let prev_values = Hashtbl.find_opt aux o_w_i.order_id |> Option.value ~default:(0.0, 0.0) in
  let prev_amount = fst prev_values in
  let prev_tax = snd prev_values in
  let amount = compute_amount o_w_i in
  let total_amount = prev_amount +. amount in
  let total_tax = prev_tax +. amount *. o_w_i.tax in
  Hashtbl.replace aux o_w_i.order_id (total_amount, total_tax)


(**
    Converts a hashtable of order totals into a list of order_total records.
    
    @param tbl The hashtable mapping order IDs to (total_amount, total_tax) tuples.
    @return A list of order_total records, each containing order_id, total_amount, and total_tax.
*)
let hashtbl_to_order_totals (tbl : (int, float * float) Hashtbl.t) : order_total list =
  Hashtbl.fold (fun order_id (total_amount, total_tax) acc ->
    { order_id; total_amount; total_tax } :: acc
  ) tbl []


(**
    Computes total amount and tax for each order based on its items.
    
    @param orders_with_items The list of order items to compute totals from.
    @return A list of records containing order_id, total_amount, and total_tax for each order.
*)
let compute_order_totals (orders_with_items : order_with_item list) : order_total list =
  let o_w_i_len = List.length orders_with_items in
  let hashtbl_ots = List.fold_left (fun aux o_w_i ->
    update_order_totals aux o_w_i;
    aux
    ) (Hashtbl.create o_w_i_len) orders_with_items 
  in
  hashtbl_to_order_totals hashtbl_ots


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
  let orders_with_items = inner_join_orders_items filtered_orders order_items in
  let order_totals = compute_order_totals orders_with_items in
  order_totals
