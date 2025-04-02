open OUnit2
open Lib.Types
open Lib.Transform

let get_order_list = [
  { id = 1; date = "2024-10"; status = Pending; origin = P } ;
  { id = 2; date = "2024-08"; status = Complete; origin = O } ;
  { id = 3; date = "2023-09"; status = Cancelled; origin = O } ;
  { id = 4; date = "2023-07"; status = Cancelled; origin = P } ;
]

let get_item_list = [
  { order_id=1; quantity=10; price=10.00; tax=0.1 } ;
  { order_id=2; quantity=7; price=100.0; tax=0.1 } ;
  { order_id=2; quantity=9; price=20.00; tax=0.1 } ;
  { order_id=4; quantity=9; price=30.00; tax=0.1 } ;
  { order_id=5; quantity=2; price=40.00; tax=0.1 } ;
]

let get_join = { order_id=1; quantity=5; price=12.00; tax=0.1; date = "2024-10"; status = Pending; origin = P }

let get_join_list = [
  { order_id=1; quantity=10; price=10.0; tax=0.1; date="2024-10"; status=Pending; origin=P };
  { order_id=2; quantity=7; price=100.0; tax=0.1; date="2024-08"; status=Complete; origin=O };
  { order_id=2; quantity=9; price=20.00; tax=0.1; date="2024-08"; status=Complete; origin=O };
  { order_id=4; quantity=9; price=30.00; tax=0.1; date="2023-07"; status=Cancelled; origin=P };
]

let get_fr_map_single = FinRecordMap.singleton "2024-10" (10.0, 1.0, 1)
let get_fr_map_multiple = 
  FinRecordMap.empty
  |> FinRecordMap.add "2024-10" (10.0, 1.0, 1)
  |> FinRecordMap.add "2024-11" (20.0, 2.0, 1)
  |> FinRecordMap.add "2024-12" (30.0, 3.0, 1)

let get_ot_map_single = OrderTotalMap.singleton 1 (100.0, 10.0)
let get_ot_map_multiple = 
  OrderTotalMap.empty
  |> OrderTotalMap.add 1 (100.0, 10.0)
  |> OrderTotalMap.add 2 (200.0, 20.0)
  |> OrderTotalMap.add 3 (300.0, 30.0)

let test_filter_orders_with_valid_statuses_and_origins _ =
  let result = filter_orders get_order_list [Pending; Complete] [P; O] in
  let expected = [
    { id = 1; date = "2024-10"; status = Pending; origin = P };
    { id = 2; date = "2024-08"; status = Complete; origin = O }
  ] in
  assert_equal ~msg:"Should return orders with matching statuses and origins"
               ~printer:(fun lst -> String.concat "," (List.map (fun o -> string_of_int o.id) lst))
               expected result

let test_filter_orders_with_empty_statuses _ =
  let result = filter_orders get_order_list [] [P; O] in
  let expected = [] in
  assert_equal ~msg:"Should return empty list when statuses is empty"
               ~printer:(fun lst -> String.concat "," (List.map (fun o -> string_of_int o.id) lst))
               expected result

let test_filter_orders_with_empty_origins _ =
  let result = filter_orders get_order_list [Pending; Complete] [] in
  let expected = [] in
  assert_equal ~msg:"Should return empty list when origins is empty"
               ~printer:(fun lst -> String.concat "," (List.map (fun o -> string_of_int o.id) lst))
               expected result

let test_filter_orders_with_no_matches _ =
  let result = filter_orders get_order_list [Pending] [O] in
  let expected = [] in
  assert_equal ~msg:"Should return empty list when no matches found"
               ~printer:(fun lst -> String.concat "," (List.map (fun o -> string_of_int o.id) lst))
               expected result

let test_inner_join_with_matching_orders_and_items _ =
let result = inner_join_orders_items get_order_list get_item_list in
let expected = get_join_list in
assert_equal ~msg:"Should join matching orders and items correctly"
              ~printer:(fun lst -> String.concat "," (List.map (fun (o: item_joined_order) -> string_of_int o.order_id) lst))
              expected result

let test_inner_join_with_no_matching_items _ =
let orders = [{ id = 6; date = "2024-01"; status = Pending; origin = P }] in
let result = inner_join_orders_items orders get_item_list in
let expected = [] in
assert_equal ~msg:"Should return empty list when no items match orders"
              ~printer:(fun lst -> String.concat "," (List.map (fun (o: item_joined_order) -> string_of_int o.order_id) lst))
              expected result

let test_inner_join_with_empty_orders _ =
let result = inner_join_orders_items [] get_item_list in
let expected = [] in
assert_equal ~msg:"Should return empty list with empty orders"
              ~printer:(fun lst -> String.concat "," (List.map (fun (o: item_joined_order) -> string_of_int o.order_id) lst))
              expected result

let test_inner_join_with_empty_items _ =
let result = inner_join_orders_items get_order_list [] in
let expected = [] in
assert_equal ~msg:"Should return empty list with empty items"
              ~printer:(fun lst -> String.concat "," (List.map (fun (o: item_joined_order) -> string_of_int o.order_id) lst))
              expected result

let test_update_order_totals_single_order _ =
  let result = update_order_totals OrderTotalMap.empty get_join in
  let expected = OrderTotalMap.singleton 1 (60.0, 6.0) in
  assert_equal ~msg:"Should correctly update totals for single order"
                ~printer:(fun m -> OrderTotalMap.bindings m |> List.map (fun (k, (a, t)) -> Printf.sprintf "%d:(%.2f,%.2f)" k a t) |> String.concat ",")
                expected result

let test_update_order_totals_multiple_items_same_order _ =
  let initial = OrderTotalMap.singleton 2 (100.0, 10.0) in
  let join1 = List.nth get_join_list 1 in
  let join2 = List.nth get_join_list 2 in
  let temp = update_order_totals initial join1 in
  let result = update_order_totals temp join2 in
  let expected = OrderTotalMap.singleton 2 (980.00, 98.00) in
  assert_equal ~msg:"Should correctly accumulate totals for multiple items of same order"
                ~printer:(fun m -> OrderTotalMap.bindings m |> List.map (fun (k, (a, t)) -> Printf.sprintf "%d:(%.2f,%.2f)" k a t) |> String.concat ",")
                expected result

let test_update_order_totals_empty_map _ =
  let result = update_order_totals OrderTotalMap.empty (List.hd get_join_list) in
  let expected = OrderTotalMap.singleton 1 (100.0, 10.0) in
  assert_equal ~msg:"Should create new entry in empty map"
                ~printer:(fun m -> OrderTotalMap.bindings m |> List.map (fun (k, (a, t)) -> Printf.sprintf "%d:(%.2f,%.2f)" k a t) |> String.concat ",")
                expected result

let test_update_financial_records_single_date _ =
  let result = update_financial_records FinRecordMap.empty get_join in
  let expected = FinRecordMap.singleton "2024-10" (12.0, 1.2, 5) in
  assert_equal ~msg:"Should correctly update totals for single date"
                ~printer:(fun m -> FinRecordMap.bindings m |> List.map (fun (k, (a, t, q)) -> Printf.sprintf "%s:(%.2f,%.2f,%d)" k a t q) |> String.concat ",")
                expected result

let test_update_financial_records_empty_map _ =
  let result = update_financial_records FinRecordMap.empty (List.hd get_join_list) in
  let expected = FinRecordMap.singleton "2024-10" (10.0, 1.0, 10) in
  assert_equal ~msg:"Should create new entry in empty map"
                ~printer:(fun m -> FinRecordMap.bindings m |> List.map (fun (k, (a, t, q)) -> Printf.sprintf "%s:(%.2f,%.2f,%d)" k a t q) |> String.concat ",")
                expected result

let test_financial_record_map_to_list_single_record _ =
  let result = financial_record_map_to_list get_fr_map_single in
  let expected = [{ period = "2024-10"; avg_rev = 10.0; avg_tax = 1.0; quantity = 1 }] in
  assert_equal ~msg:"Should convert single record map to list"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> r.period ^ ":" ^ string_of_float r.avg_rev ^ ":" ^ string_of_float r.avg_tax) lst))
                expected result

let test_financial_record_map_to_list_multiple_records _ =
  let result = financial_record_map_to_list get_fr_map_multiple in
  let expected = [
    { period = "2024-12"; avg_rev = 30.0; avg_tax = 3.0; quantity = 1 };
    { period = "2024-11"; avg_rev = 20.0; avg_tax = 2.0; quantity = 1 };
    { period = "2024-10"; avg_rev = 10.0; avg_tax = 1.0; quantity = 1 }
  ] in
  assert_equal ~msg:"Should convert multiple record map to list"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> r.period ^ ":" ^ string_of_float r.avg_rev ^ ":" ^ string_of_float r.avg_tax) lst))
                expected result

let test_financial_record_map_to_list_empty_map _ =
  let result = financial_record_map_to_list FinRecordMap.empty in
  let expected = [] in
  assert_equal ~msg:"Should return empty list for empty map"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> r.period ^ ":" ^ string_of_float r.avg_rev ^ ":" ^ string_of_float r.avg_tax) lst))
                expected result

let test_order_total_map_to_list_single_order _ =
  let result = order_total_map_to_list get_ot_map_single in
  let expected = [{ order_id = 1; total_amount = 100.0; total_tax = 10.0 }] in
  assert_equal ~msg:"Should convert single order map to list"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> string_of_int r.order_id ^ ":" ^ string_of_float r.total_amount ^ ":" ^ string_of_float r.total_tax) lst))
                expected result

let test_order_total_map_to_list_multiple_orders _ =
  let result = order_total_map_to_list get_ot_map_multiple in
  let expected = [
    { order_id = 3; total_amount = 300.0; total_tax = 30.0 };
    { order_id = 2; total_amount = 200.0; total_tax = 20.0 };
    { order_id = 1; total_amount = 100.0; total_tax = 10.0 }
  ] in
  assert_equal ~msg:"Should convert multiple order map to list"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> string_of_int r.order_id ^ ":" ^ string_of_float r.total_amount ^ ":" ^ string_of_float r.total_tax) lst))
                expected result

let test_order_total_map_to_list_empty_map _ =
  let result = order_total_map_to_list OrderTotalMap.empty in
  let expected = [] in
  assert_equal ~msg:"Should return empty list for empty map"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> string_of_int r.order_id ^ ":" ^ string_of_float r.total_amount ^ ":" ^ string_of_float r.total_tax) lst))
                expected result

let test_compute_order_totals_with_valid_data _ =
  let result = compute_order_totals get_join_list in
  let expected = [
    { order_id = 1; total_amount = 100.00; total_tax = 10.00 };
    { order_id = 2; total_amount = 880.00; total_tax = 88.00 };
    { order_id = 4; total_amount = 270.00; total_tax = 27.00 };
  ] in
  assert_equal ~msg:"Should compute order totals correctly for multiple orders"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> string_of_int r.order_id ^ ":" ^ string_of_float r.total_amount ^ ":" ^ string_of_float r.total_tax) lst))
                expected result

let test_compute_order_totals_with_empty_list _ =
  let result = compute_order_totals [] in
  let expected = [] in
  assert_equal ~msg:"Should return empty list for empty input"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> string_of_int r.order_id ^ ":" ^ string_of_float r.total_amount ^ ":" ^ string_of_float r.total_tax) lst))
                expected result

let test_compute_order_totals_with_single_item _ =
  let result = compute_order_totals [get_join] in
  let expected = [{ order_id = 1; total_amount = 60.0; total_tax = 6.0 }] in
  assert_equal ~msg:"Should compute order totals correctly for single item"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> string_of_int r.order_id ^ ":" ^ string_of_float r.total_amount ^ ":" ^ string_of_float r.total_tax) lst))
                expected result

let get_join_list = [
  { order_id=1; quantity=10; price=10.0; tax=0.1; date="2024-10"; status=Pending; origin=P };
  { order_id=2; quantity=7; price=100.0; tax=0.1; date="2024-08"; status=Complete; origin=O };
  { order_id=2; quantity=9; price=20.00; tax=0.1; date="2024-08"; status=Complete; origin=O };
  { order_id=4; quantity=9; price=30.00; tax=0.1; date="2023-07"; status=Cancelled; origin=P };
]
              
let test_compute_financial_records_with_valid_data _ =
  let result = compute_financial_records get_join_list in
  let expected = [
    { period = "2024-10"; avg_rev = 10.0; avg_tax = 1.0; quantity = 10 };
    { period = "2024-08"; avg_rev = 55.00; avg_tax = 5.50; quantity = 16 };
    { period = "2023-07"; avg_rev = 30.00; avg_tax = 3.00; quantity = 9 };
  ] in
  assert_equal ~msg:"Should compute financial records correctly for multiple dates"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> r.period ^ ":" ^ string_of_float r.avg_rev ^ ":" ^ string_of_float r.avg_tax) lst))
                expected result

let test_compute_financial_records_with_empty_list _ =
  let result = compute_financial_records [] in
  let expected = [] in
  assert_equal ~msg:"Should return empty list for empty input"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> r.period ^ ":" ^ string_of_float r.avg_rev ^ ":" ^ string_of_float r.avg_tax) lst))
                expected result

let test_compute_financial_records_with_single_item _ =
  let result = compute_financial_records [get_join] in
  let expected = [{ period = "2024-10"; avg_rev = 12.0; avg_tax = 1.2; quantity = 5 }] in
  assert_equal ~msg:"Should compute order totals correctly for single item"
                ~printer:(fun lst -> String.concat "," (List.map (fun r -> r.period ^ ":" ^ string_of_float r.avg_rev ^ ":" ^ string_of_float r.avg_tax) lst))
                expected result

  let suite =
    "Transform Tests" >:::
    [
      "filter_orders tests" >:::
      [
        "test_filter_orders_with_valid_statuses_and_origins" >:: test_filter_orders_with_valid_statuses_and_origins;
        "test_filter_orders_with_empty_statuses" >:: test_filter_orders_with_empty_statuses;
        "test_filter_orders_with_empty_origins" >:: test_filter_orders_with_empty_origins;
        "test_filter_orders_with_no_matches" >:: test_filter_orders_with_no_matches ;
      ];
  
      "inner_join_orders_items tests" >:::
      [
        "test_inner_join_with_matching_orders_and_items" >:: test_inner_join_with_matching_orders_and_items;
        "test_inner_join_with_no_matching_items" >:: test_inner_join_with_no_matching_items;
        "test_inner_join_with_empty_orders" >:: test_inner_join_with_empty_orders;
        "test_inner_join_with_empty_items" >:: test_inner_join_with_empty_items;
      ];
  
      "update_order_totals tests" >:::
      [
        "test_update_order_totals_single_order" >:: test_update_order_totals_single_order;
        "test_update_order_totals_multiple_items_same_order" >:: test_update_order_totals_multiple_items_same_order;
        "test_update_order_totals_empty_map" >:: test_update_order_totals_empty_map;
      ];
  
      "update_financial_records tests" >:::
      [
        "test_update_financial_records_single_date" >:: test_update_financial_records_single_date;
        "test_update_financial_records_empty_map" >:: test_update_financial_records_empty_map;
      ];
  
      "financial_record_map_to_list tests" >:::
      [
        "test_financial_record_map_to_list_single_record" >:: test_financial_record_map_to_list_single_record;
        "test_financial_record_map_to_list_multiple_records" >:: test_financial_record_map_to_list_multiple_records;
        "test_financial_record_map_to_list_empty_map" >:: test_financial_record_map_to_list_empty_map;
      ];
  
      "order_total_map_to_list tests" >:::
      [
        "test_order_total_map_to_list_single_order" >:: test_order_total_map_to_list_single_order;
        "test_order_total_map_to_list_multiple_orders" >:: test_order_total_map_to_list_multiple_orders;
        "test_order_total_map_to_list_empty_map" >:: test_order_total_map_to_list_empty_map;
      ];
  
      "compute_order_totals tests" >:::
      [
        "test_compute_order_totals_with_valid_data" >:: test_compute_order_totals_with_valid_data;
        "test_compute_order_totals_with_empty_list" >:: test_compute_order_totals_with_empty_list;
        "test_compute_order_totals_with_single_item" >:: test_compute_order_totals_with_single_item;
      ];
  
      "compute_financial_records tests" >:::
      [
        "test_compute_financial_records_with_valid_data" >:: test_compute_financial_records_with_valid_data;
        "test_compute_financial_records_with_empty_list" >:: test_compute_financial_records_with_empty_list;
        "test_compute_financial_records_with_single_item" >:: test_compute_financial_records_with_single_item;
      ];
    ]
  
  let () = run_test_tt_main suite