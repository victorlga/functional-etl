open Parse
open Types

let ( let* ) = Result.bind


let filter_orders_by_statuses orders statuses =
  List.filter (fun (order: order) -> List.mem order.status statuses) orders


let filter_orders_by_origins orders origins =
  List.filter (fun (order: order) -> List.mem order.origin origins) orders


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


let compute_amount o_w_i =
  o_w_i.price *. float_of_int o_w_i.quantity


let update_order_totals aux (o_w_i : order_with_item) =
  let prev_values = OrderTotalMap.find_opt o_w_i.order_id aux |> Option.value ~default:(0.0, 0.0) in
  let prev_amount, prev_tax = prev_values in
  let amount = compute_amount o_w_i in
  let total_amount = prev_amount +. amount in
  let total_tax = prev_tax +. amount *. o_w_i.tax in
  OrderTotalMap.add o_w_i.order_id (total_amount, total_tax) aux


let order_total_map_to_list map =
  OrderTotalMap.fold (fun order_id (total_amount, total_tax) acc ->
    { order_id; total_amount; total_tax } :: acc
  ) map []


let compute_order_totals orders_with_items =
  List.fold_left (fun aux o_w_i ->
    update_order_totals aux o_w_i
  ) OrderTotalMap.empty orders_with_items 
  |> order_total_map_to_list


let update_financial_records aux (o_w_i : order_with_item) =
  let prev_values = FinRecordMap.find_opt o_w_i.date aux |> Option.value ~default:(0.0, 0.0) in
  let prev_revenue, prev_tax = prev_values in
  let amount = compute_amount o_w_i in
  let total_revenue = prev_revenue +. amount in
  let total_tax = prev_tax +. amount *. o_w_i.tax in
  FinRecordMap.add o_w_i.date (total_revenue, total_tax) aux
  
  
let financial_record_map_to_list map =
  FinRecordMap.fold (fun period (revenue, tax) acc ->
    { period; revenue; tax } :: acc
  ) map []


let compute_financial_records orders_with_items =
  List.fold_left (fun aux o_w_i ->
    update_financial_records aux o_w_i
  ) FinRecordMap.empty orders_with_items
  |> financial_record_map_to_list
  |> List.sort (fun r1 r2 -> String.compare r1.period r2.period)
  |> List.rev


let parse_and_filter_orders raw_orders criterias = 
  let (statuses, origins) = criterias in
  let* orders = parse_order_list raw_orders in
  let* parsed_statuses = parse_status_list statuses in
  let* parsed_origins = parse_origin_list origins in
  let filtered_orders = filter_orders_by_statuses orders parsed_statuses in
  let filtered_orders = filter_orders_by_origins filtered_orders parsed_origins in
  Ok filtered_orders


let transform data criterias =
  let (raw_orders, raw_order_items) = data in
  let* filtered_orders = parse_and_filter_orders raw_orders criterias in
  let* order_items = parse_order_item_list raw_order_items in
  let orders_with_items = inner_join_orders_items filtered_orders order_items in
  let order_totals = compute_order_totals orders_with_items in
  let financial_records = compute_financial_records orders_with_items in
  Ok (order_totals, financial_records)
