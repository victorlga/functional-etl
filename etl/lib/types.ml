type status = Pending | Complete | Cancelled
type origin = O | P

type order = {
  id : int;
  status : status;
  origin : origin;
}

type order_item = {
  order_id : int;
  quantity : int;
  price : float;
  tax : float;
}

(* type t = {
  order_id : int;
  quantity : int;
  price : float;
  tax : float;
  status : status;
  origin : origin;
} *)

type order_totals = {
  order_id : int;
  total_amount : float;
  total_tax : float;
}