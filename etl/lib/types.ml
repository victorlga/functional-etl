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

type order_with_item = {
  order_id : int;
  status : status;
  origin : origin;
  quantity : int;
  price : float;
  tax : float;
}

type order_total = {
  order_id : int;
  total_amount : float;
  total_tax : float;
}

module OrderTotalMap = Map.Make(Int)