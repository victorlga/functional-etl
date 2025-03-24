open OUnit2
open Lib.Parse
open Lib.Types

let test_parse_status_pending _ =
  let result = parse_status "Pending" in
  assert_equal 
    ~msg:"Should parse 'Pending' as Pending status"
    ~printer:(function
      | Ok Pending -> "Ok Pending"
      | Ok Complete -> "Ok Complete"
      | Ok Cancelled -> "Ok Cancelled"
      | Error s -> "Error: " ^ s)
    (Ok Pending) result

let test_parse_status_complete _ =
  let result = parse_status "Complete" in
  assert_equal 
    ~msg:"Should parse 'Complete' as Complete status"
    ~printer:(function
      | Ok Pending -> "Ok Pending"
      | Ok Complete -> "Ok Complete"
      | Ok Cancelled -> "Ok Cancelled"
      | Error s -> "Error: " ^ s)
    (Ok Complete) result

let test_parse_status_cancelled _ =
  let result = parse_status "Cancelled" in
  assert_equal 
    ~msg:"Should parse 'Cancelled' as Cancelled status"
    ~printer:(function
      | Ok Pending -> "Ok Pending"
      | Ok Complete -> "Ok Complete"
      | Ok Cancelled -> "Ok Cancelled"
      | Error s -> "Error: " ^ s)
    (Ok Cancelled) result

let test_parse_status_unknown _ =
  let result = parse_status "Invalid" in
  assert_equal 
    ~msg:"Should return error for unknown status 'Invalid'"
    ~printer:(function
      | Ok Pending -> "Ok Pending"
      | Ok Complete -> "Ok Complete"
      | Ok Cancelled -> "Ok Cancelled"
      | Error s -> "Error: " ^ s)
    (Error "Unknown status: Invalid") result

let test_parse_origin_o _ =
  let result = parse_origin "O" in
  assert_equal 
    ~msg:"Should parse 'O' as O origin"
    ~printer:(function
      | Ok O -> "Ok O"
      | Ok P -> "Ok P"
      | Error s -> "Error: " ^ s)
    (Ok O) result

let test_parse_origin_p _ =
  let result = parse_origin "P" in
  assert_equal 
    ~msg:"Should parse 'P' as P origin"
    ~printer:(function
      | Ok O -> "Ok O"
      | Ok P -> "Ok P"
      | Error s -> "Error: " ^ s)
    (Ok P) result

let test_parse_origin_unknown _ =
  let result = parse_origin "A" in
  assert_equal 
    ~msg:"Should return error for unknown origin 'A'"
    ~printer:(function
      | Ok O -> "Ok O"
      | Ok P -> "Ok P"
      | Error s -> "Error: " ^ s)
    (Error "Unknown origin: A") result
    
let test_parse_int _ =
  let result = parse_int "" "10" in
  assert_equal 
    ~msg:"Should parse '10' as integer 10"
    ~printer:(function
      | Ok n -> "Ok " ^ string_of_int n
      | Error s -> "Error: " ^ s)
    (Ok 10) result
    
let test_parse_int_invalid_number _ =
  let result = parse_int "id" "10.0" in
  assert_equal 
    ~msg:"Should return error for non-integer '10.0' in id field"
    ~printer:(function
      | Ok n -> "Ok " ^ string_of_int n
      | Error s -> "Error: " ^ s)
    (Error "Invalid id: 10.0") result

let test_parse_int_invalid_str _ =
  let result = parse_int "id" "str" in
  assert_equal 
    ~msg:"Should return error for non-numeric 'str' in id field"
    ~printer:(function
      | Ok n -> "Ok " ^ string_of_int n
      | Error s -> "Error: " ^ s)
    (Error "Invalid id: str") result

let test_parse_float _ =
  let result = parse_float "" "10.0" in
  assert_equal 
    ~msg:"Should parse '10.0' as float 10.0"
    ~printer:(function
      | Ok f -> "Ok " ^ string_of_float f
      | Error s -> "Error: " ^ s)
    (Ok 10.0) result

let test_parse_float_wo_decimals _ =
  let result = parse_float "price" "10" in
  assert_equal 
    ~msg:"Should parse integer '10' as float 10.0 in price field"
    ~printer:(function
      | Ok f -> "Ok " ^ string_of_float f
      | Error s -> "Error: " ^ s)
    (Ok 10.0) result

let test_parse_float_invalid_str _ =
  let result = parse_float "price" "str" in
  assert_equal 
    ~msg:"Should return error for non-numeric 'str' in price field"
    ~printer:(function
      | Ok f -> "Ok " ^ string_of_float f
      | Error s -> "Error: " ^ s)
    (Error "Invalid price: str") result

let test_parse_date _ =
  let result = parse_date "2024-10-02T03:05:39" in
  assert_equal 
    ~msg:"Should parse '2024-10-02T03:05:39' to '2024-10'"
    ~printer:(function
      | Ok s -> "Ok " ^ s
      | Error s -> "Error: " ^ s)
    (Ok "2024-10") result
  
let test_parse_date_invalid_separator _ =
  let result = parse_date "2024/10/02T03:05:39" in
  assert_equal 
    ~msg:"Should return error for invalid separator in '2024/10/02T03:05:39'"
    ~printer:(function
      | Ok s -> "Ok " ^ s
      | Error s -> "Error: " ^ s)
    (Error "Invalid date format: 2024/10 (expected YYYY-MM-DDTHH:mm:SS)") result
  
let test_parse_date_invalid_order _ =
  let result = parse_date "02-10-2024T03:05:39" in
  assert_equal 
    ~msg:"Should return error for invalid order in '02-10-2024T03:05:39'"
    ~printer:(function
      | Ok s -> "Ok " ^ s
      | Error s -> "Error: " ^ s)
    (Error "Invalid date format: 02-10-2 (expected YYYY-MM-DDTHH:mm:SS)") result

let test_parse_date_invalid_format _ =
  let result = parse_date "24-10-02T03:05:39" in
  assert_equal 
    ~msg:"Should return error for invalid format in '24-10-02T03:05:39'"
    ~printer:(function
      | Ok s -> "Ok " ^ s
      | Error s -> "Error: " ^ s)
    (Error "Invalid date format: 24-10-0 (expected YYYY-MM-DDTHH:mm:SS)") result

let mock_parser s =
  if String.length s >= 2 && String.sub s 0 2 = "ok" then
    Ok (String.sub s 2 (String.length s - 2))  (* Returns rest of string *)
  else
    Error (Printf.sprintf "Failed to parse: %s" s)

let test_parse_list _ =
  let inputs = ["okA"; "okB"; "okC"] in
  let result = parse_list mock_parser inputs in
  assert_equal 
    ~msg:"Should parse list ['okA'; 'okB'; 'okC'] to ['A'; 'B'; 'C']"
    ~printer:(function
      | Ok lst -> "Ok [" ^ String.concat "; " lst ^ "]"
      | Error s -> "Error: " ^ s)
    (Ok ["A"; "B"; "C"]) result
    
let test_parse_list_with_invalid _ =
  let inputs = ["okA"; "bad"; "okC"] in
  let result = parse_list mock_parser inputs in
  assert_equal 
    ~msg:"Should return error for invalid input 'bad' in list ['okA'; 'bad'; 'okC']"
    ~printer:(function
      | Ok lst -> "Ok [" ^ String.concat "; " lst ^ "]"
      | Error s -> "Error: " ^ s)
    (Error "Failed to parse: bad") result

let test_parse_list_empty _ =
  let inputs = [] in
  let result = parse_list mock_parser inputs in
  assert_equal 
    ~msg:"Should parse empty list to empty list"
    ~printer:(function
      | Ok lst -> "Ok [" ^ String.concat "; " lst ^ "]"
      | Error s -> "Error: " ^ s)
    (Ok []) result

(* Test suite *)
let suite =
  "Parse Tests" >:::
  [
    "parse_status tests" >:::
    [
      "parse_status with Pending" >:: test_parse_status_pending;
      "parse_status with Complete" >:: test_parse_status_complete;
      "parse_status with Cancelled" >:: test_parse_status_cancelled;
      "parse_status with unknown" >:: test_parse_status_unknown;
    ];

    "parse_origin tests" >:::
    [
      "parse_origin with O" >:: test_parse_origin_o;
      "parse_origin with P" >:: test_parse_origin_p;
      "parse_origin with unknown" >:: test_parse_origin_unknown;
    ];

    "parse_int tests" >:::
    [
      "parse_int with Int" >:: test_parse_int;
      "parse_int with invalid number" >:: test_parse_int_invalid_number;
      "parse_int with invalid String" >:: test_parse_int_invalid_str;
    ];

    "parse_float tests" >:::
    [
      "parse_float with Float" >:: test_parse_float;
      "parse_float without decimals" >:: test_parse_float_wo_decimals;
      "parse_float with invalid String" >:: test_parse_float_invalid_str;
    ];

    "parse_date tests" >:::
    [
      "parse_date" >:: test_parse_date;
      "parse_date with invalid separator" >:: test_parse_date_invalid_separator;
      "parse_date with invalid order" >:: test_parse_date_invalid_order;
      "parse_date with invalid format" >:: test_parse_date_invalid_format;
    ];

    "parse_list tests" >:::
    [
      "parse_list with all valid inputs" >:: test_parse_list;
      "parse_list with one invalid input" >:: test_parse_list_with_invalid;
      "parse_list with empty list" >:: test_parse_list_empty;
    ];
  ]

(* Run the tests *)
let () = run_test_tt_main suite