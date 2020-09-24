(** Expression checks *)
let expr_checks = [
  Equality.EqList.name
; Equality.EqOption.name
; Match.MatchBool.name
; Match.MatchInt.name
; Match.MatchListVerbose.name
; Match.MatchRecord.name
; Match.MatchTuple.name
; Verbose.LitPrepend.name
; Verbose.TupleProj.name
; Verbose.IfReturnsLit.name
; Verbose.IfCondThenCond.name
; Verbose.IfNotCond.name
; Verbose.IfToOr.name
; Verbose.IfToAnd.name
; Verbose.IfToAndInv.name
; Verbose.IfToOrInv.name
; Verbose.NestedIf.name
; Verbose.NestedMatch.name
; Verbose.RedundantOr.name
; Verbose.RedundantAnd.name
; Verbose.EqBool.name
]

(** Top-level structure checks *)
let struct_checks = [
  Hof.UseMap.name
; Hof.UseFold.name
; Hof.UseIter.name
]

(** Lexical checks *)
let lexical_checks = [
  Lexical.LineLength.name
]
