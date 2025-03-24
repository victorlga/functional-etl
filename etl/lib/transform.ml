open Parse
open Types

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Filters orders by a list of statuses.
    @param orders List of order records
    @param statuses List of Status variants to filter by
    @param origins List of Origin variants to filter by
    @return Filtered list of orders matching the specified statuses and origins *)
let filter_orders orders statuses origins =
  List.filter (fun (order: order) -> List.mem order.status statuses) orders
  |> List.filter (fun (order: order) -> List.mem order.origin origins)

(** Performs an inner join between orders and order items.
    @param orders List of order records
    @param order_items List of order item records
    @return List of combined item_joined_order records *)
let inner_join_orders_items orders order_items =
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

(** Generic function to update a map with totals based on item_joined_order.
    @param find_opt Map's find_opt function to look up existing values
    @param add Map's add function to store updated values
    @param get_key Function to extract the key from item_joined_order
    @param aux Existing map containing previous totals
    @param join item_joined_order record with data to process
    @return Updated map with new calculated totals *)
let update_totals ~find_opt ~add ~get_key aux (join : item_joined_order) =
  let prev_values = find_opt (get_key join) aux |> Option.value ~default:(0.0, 0.0) in
  let prev_amount, prev_tax = prev_values in
  let amount = join.price *. float_of_int join.quantity in
  let total_amount = prev_amount +. amount in
  let total_tax = prev_tax +. amount *. join.tax in
  add (get_key join) (total_amount, total_tax) aux

(** Updates order totals in a map with a new item_joined_order.
    @param aux Existing OrderTotalMap with previous order totals
    @param join item_joined_order record containing order data to process
    @return Updated OrderTotalMap with new order totals *)
let update_order_totals aux join =
  update_totals
    ~find_opt:OrderTotalMap.find_opt
    ~add:OrderTotalMap.add
    ~get_key:(fun j -> j.order_id)
    aux join

(** Updates financial records in a map with a new item_joined_order.
    @param aux Existing FinRecordMap with previous financial totals
    @param join item_joined_order record containing financial data to process
    @return Updated FinRecordMap with new financial totals *)
let update_financial_records aux join =
  update_totals
    ~find_opt:FinRecordMap.find_opt
    ~add:FinRecordMap.add
    ~get_key:(fun j -> j.date)
    aux join

(** Converts a map to a list of records using a provided conversion function.
    @param fold The fold function for the specific map type
    @param make_record Function to create a record from key and value
    @param map The input map to convert
    @return List of records *)
let map_to_list ~fold ~make_record map =
  fold (fun key value acc ->
    (make_record key value) :: acc
  ) map []

(** Converts a FinRecordMap to a list of financial_record records.
    @param map FinRecordMap containing revenue and tax data
    @return List of financial_record records *)
let financial_record_map_to_list map =
  map_to_list 
    ~fold:FinRecordMap.fold
    ~make_record:(fun period (revenue, tax) -> { period; revenue; tax })
    map

(** Converts an OrderTotalMap to a list of order_total records.
    @param map OrderTotalMap containing total amounts and taxes
    @return List of order_total records *)
let order_total_map_to_list map =
  map_to_list
    ~fold:OrderTotalMap.fold
    ~make_record:(fun order_id (total_amount, total_tax) -> 
      { order_id; total_amount; total_tax })
    map

(** Computes order totals from a list of orders with items.
    @param items_joined_orders List of item_joined_order records
    @return List of computed order_total records *)
let compute_order_totals items_joined_orders =
  List.fold_left (fun aux join ->
    update_order_totals aux join
  ) OrderTotalMap.empty items_joined_orders
  |> order_total_map_to_list
  |> List.rev

(** Computes financial records from a list of orders with items.
    @param items_joined_orders List of item_joined_order records
    @return Sorted list of financial_record records (most recent first) *)
let compute_financial_records items_joined_orders =
  List.fold_left (fun aux join ->
    update_financial_records aux join
  ) FinRecordMap.empty items_joined_orders
  |> financial_record_map_to_list
  |> List.sort (fun r1 r2 -> String.compare r2.period r1.period)

(** Parses raw orders and filters them by criteria.
    @param raw_orders List of raw CSV rows for orders
    @param criterias Tuple of status strings and origin strings
    @return Result containing filtered list of parsed orders or error string *)
let parse_and_filter_orders raw_orders criterias = 
  let (statuses, origins) = criterias in
  let* orders = parse_order_list raw_orders in
  let* parsed_statuses = parse_status_list statuses in
  let* parsed_origins = parse_origin_list origins in
  let filtered_orders = filter_orders orders parsed_statuses parsed_origins in
  Ok filtered_orders

(** Transforms raw data into order totals and financial records.
    @param data Tuple of raw orders and order items from CSV
    @param criterias Tuple of status and origin filters
    @return Result containing tuple of order_totals and financial_records or error string *)
let transform data criterias =
  let (raw_orders, raw_order_items) = data in
  let* filtered_orders = parse_and_filter_orders raw_orders criterias in
  let* order_items = parse_order_item_list raw_order_items in
  let items_joined_orders = inner_join_orders_items filtered_orders order_items in
  let order_totals = compute_order_totals items_joined_orders in
  let financial_records = compute_financial_records items_joined_orders in
  Ok (order_totals, financial_records)