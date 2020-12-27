(** This module exposes two methods for traversing an abstract syntax tree using a custom iterator 
*)

module Iter = Iterator
(* Use a default config - we'll parse this in main and change it *)
let current_config : Config.config ref = Config.default
