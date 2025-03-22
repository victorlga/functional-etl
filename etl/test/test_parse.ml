open OUnit2
open Lib.Parse
open Lib.Types

let test_parse_status_pending _ =
  let result = parse_status "Pending" in
  assert_equal (Ok Pending) result

let test_parse_status_complete _ =
  let result = parse_status "Complete" in
  assert_equal (Ok Complete) result

let test_parse_status_cancelled _ =
  let result = parse_status "Cancelled" in
  assert_equal (Ok Cancelled) result

let test_parse_status_unknown _ =
  let result = parse_status "Invalid" in
  assert_equal (Error "Unknown status: Invalid") result

let test_parse_origin_o _ =
  let result = parse_origin "O" in
  assert_equal (Ok O) result

let test_parse_origin_p _ =
  let result = parse_origin "P" in
  assert_equal (Ok P) result

let test_parse_origin_unknown _ =
  let result = parse_origin "A" in
  assert_equal (Error "Unknown origin: A") result

let test_parse_int _ =
  let result = parse_int "" "10" in
  assert_equal (Ok 10) result

let test_parse_int_invalid_number _ =
  let result = parse_int "id" "10.0" in
  assert_equal (Error "Invalid id: 10.0") result

let test_parse_int_invalid_str _ =
  let result = parse_int "id" "str" in
  assert_equal (Error "Invalid id: str") result

let test_parse_float _ =
  let result = parse_float "" "10.0" in
  assert_equal (Ok 10.0) result

let test_parse_float_wo_decimals _ =
  let result = parse_float "price" "10" in
  assert_equal (Ok 10.0) result

let test_parse_float_invalid_str _ =
  let result = parse_float "price" "str" in
  assert_equal (Error "Invalid price: str") result

let test_parse_date _ =
  let result = parse_date "2024-10-02T03:05:39" in
  assert_equal (Ok "2024-10") result

let test_parse_date_invalid_separator _ =
  let result = parse_date "2024/10/02T03:05:39" in
  assert_equal (Error "Invalid date format: 2024/10 (expected YYYY-MM-DDTHH:mm:SS)") result

let test_parse_date_invalid_order _ =
  let result = parse_date "02-10-2024T03:05:39" in
  assert_equal (Error "Invalid date format: 02-10-2 (expected YYYY-MM-DDTHH:mm:SS)") result

let test_parse_date_invalid_format _ =
  let result = parse_date "24-10-02T03:05:39" in
  assert_equal (Error "Invalid date format: 24-10-0 (expected YYYY-MM-DDTHH:mm:SS)") result

let mock_parser s =
  if String.length s >= 2 && String.sub s 0 2 = "ok" then
    Ok (String.sub s 2 (String.length s - 2))  (* Returns rest of string *)
  else
    Error (Printf.sprintf "Failed to parse: %s" s)

let test_parse_list_all_valid _ =
  let inputs = ["okA"; "okB"; "okC"] in
  let result = parse_list mock_parser inputs in
  assert_equal (Ok ["A"; "B"; "C"]) result

let test_parse_list_with_invalid _ =
  let inputs = ["okA"; "bad"; "okC"] in
  let result = parse_list mock_parser inputs in
  assert_equal (Error "Failed to parse: bad") result

let test_parse_list_empty _ =
  let inputs = [] in
  let result = parse_list mock_parser inputs in
  assert_equal (Ok []) result

(* Test suite *)
let suite =
  "Parse Tests" >:::
  [
    "parse_status with Pending"   >:: test_parse_status_pending ;
    "parse_status with Complete"  >:: test_parse_status_complete ;
    "parse_status with Cancelled" >:: test_parse_status_cancelled ;
    "parse_status with unknown"   >:: test_parse_status_unknown ;
    "parse_origin with O" >:: test_parse_origin_o ;
    "parse_origin with P" >:: test_parse_origin_p ;
    "parse_origin with unknown" >:: test_parse_origin_unknown ;
    "parse_int with Int" >:: test_parse_int ;
    "parse_int with invalid number" >:: test_parse_int_invalid_number ;
    "parse_int with invalid String" >:: test_parse_int_invalid_str ;
    "parse_float with Float" >:: test_parse_float ;
    "parse_float without decimals" >:: test_parse_float_wo_decimals ;
    "parse_float with invalid String" >:: test_parse_float_invalid_str ;
    "parse_date" >:: test_parse_date ;
    "parse_date with invalid separator" >:: test_parse_date_invalid_separator ;
    "parse_date with invalid order" >:: test_parse_date_invalid_order ;
    "parse_date with invalid format" >:: test_parse_date_invalid_format ;
    "parse_list with all valid inputs" >:: test_parse_list_all_valid ;
    "parse_list with one invalid input" >:: test_parse_list_with_invalid ;
    "parse_list with empty list" >:: test_parse_list_empty ;
  ]

(* Run the tests *)
let () = run_test_tt_main suite