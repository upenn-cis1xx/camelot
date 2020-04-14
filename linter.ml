open Parsetree
open Style
(** Modify this file, checkers/report.ml,
    checkers/custom.ml 
    to extend the linter with custom rules
*)


(** This is the handler function passed into linterator 
    It extracts patterns of interest so that we can match
    them against the predefined rules in checkers
*)

let all_hints : Style.hint list ref = ref []

let patterns_of_interest (Parsetree.expression) : Report.pattern = 
  let 

    let 



