let expr_checks = [
  Equality.EqList.check
; Equality.EqOption.check
; Lexical.LineLength.check
; Match.MatchBool.check
; Match.MatchInt.check
; Match.MatchListVerbose.check
; Match.MatchRecord.check
; Match.MatchTuple.check
; Verbose.LitPrepend.check
; Verbose.TupleProj.check
; Verbose.IfReturnsLit.check
; Verbose.IfCondThenCond.check
; Verbose.IfNotCond.check
; Verbose.IfToOr.check
; Verbose.IfToAnd.check
; Verbose.IfToAndInv.check
; Verbose.IfToOrInv.check
; Verbose.NestedIf.check
; Verbose.NestedMatch.check
]

let struct_checks = [
  Hof.UseMap.check
]
