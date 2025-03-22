(** Represents the status of an order *)
type status = 
  | Pending   (** Order is awaiting processing *)
  | Complete  (** Order has been fully processed *)
  | Cancelled (** Order has been cancelled *)

(** Represents the origin of an order *)
type origin = 
  | O (** Origin type O *)
  | P (** Origin type P *)

(** Map module for storing order totals, keyed by integer order ID *)
module OrderTotalMap = Map.Make(Int)

(** Map module for storing financial records, keyed by string period *)
module FinRecordMap = Map.Make(String)

(** Represents an order in the system *)
type order = {
  id : int;        (** Unique identifier for the order *)
  date : string;   (** Date of the order in YYYY-MM format *)
  status : status; (** Current status of the order *)
  origin : origin; (** Origin of the order *)
}

(** Represents an item within an order *)
type order_item = {
  order_id : int;  (** ID of the order this item belongs to *)
  quantity : int;  (** Number of units of this item *)
  price : float;   (** Price per unit *)
  tax : float;     (** Tax rate applied to this item *)
}

(** Represents a combined order and item record for processing *)
type order_with_item = {
  order_id : int;  (** ID of the order *)
  date : string;   (** Date of the order in YYYY-MM format *)
  status : status; (** Current status of the order *)
  origin : origin; (** Origin of the order *)
  quantity : int;  (** Number of units *)
  price : float;   (** Price per unit *)
  tax : float;     (** Tax rate applied *)
}

(** Represents the total amounts for an order *)
type order_total = {
  order_id : int;      (** ID of the order *)
  total_amount : float; (** Total amount including price and quantity *)
  total_tax : float;    (** Total tax amount for the order *)
}

(** Represents a financial record for a period *)
type financial_record = {
  period : string; (** Time period (typically YYYY-MM) *)
  revenue : float; (** Total revenue for the period *)
  tax : float;     (** Total tax collected for the period *)
}