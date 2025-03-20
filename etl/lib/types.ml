type status = Pending | Complete | Cancelled
type origin = O | P

module OrderTotalMap = Map.Make(Int)

type order = {
  id : int;
  date : string;
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
  date : string;
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

type financial_record = {
  period: string;
  revenue: float;
  tax: float;
}
