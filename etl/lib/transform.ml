open Types

let filter_orders_by_statuses orders statuses =
  List.filter (fun order -> List.mem order.status statuses) orders

let filter_orders_by_origins orders origins =
  List.filter (fun order -> List.mem order.origin origins) orders

let compute_totals (order_items : order_item list) order_id =
  let matching_items = List.filter (fun (item : order_item) -> item.order_id = order_id) order_items in
  List.fold_left
    (fun (amount, tax) item ->
      let item_amount = item.price *. float_of_int item.quantity in
      (amount +. item_amount, tax +. item_amount *. item.tax))
    (0.0, 0.0) matching_items

let compute_order_totals orders order_items =
  List.map
    (fun order ->
      let total_amount, total_tax = compute_totals order_items order.id in
      { order_id = order.id; total_amount; total_tax })
    orders