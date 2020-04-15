(** The linters interface. You need to provide the following vals
    -- rules : a list of functions that allow you to generate suggestions
    -- patterns_of_interest : a function that is passed to the iterator
       Given a filename and parsetree expression, you can configure this
       method to write to a database/internal datastructure as a side - effect.
    -- lint : a function that should produce a set of hints that you store somewhere
    -- hint : a function that should print the hints you stored in lint

*)

(* List of rules to lint *)
val rules : (Style.hint list ref -> Style.patternctxt -> unit) list 

(* Parses patterns of interest - can store these anywhere you like or not at all *)
val patterns_of_interest : string -> Parsetree.expression -> unit

(* Checks rules against the patterns found *)
val lint : unit -> unit

(* Prints linter contents *)
val hint : unit -> unit
