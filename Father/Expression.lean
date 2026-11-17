import Father.Comparable

-- Expresions are lines that could potentially return something,
-- even if, like Code, they have side effects when evaluated

inductive Expression where
  | functionCall (called : String) (arguments : List Expression) : Expression
  | fromDB (query : String) : Expression
  | unspecified (literal : String) : Expression

open Expression
def exampleExpr := functionCall "authorize" [unspecified "@written_conversation"]

def Expression.toString : Expression → String
  | functionCall called arguments => s!"{called}({", ".intercalate (arguments.map Expression.toString)})"
  | fromDB query => query
  | unspecified literal => literal

instance : ToString Expression := ⟨Expression.toString⟩

instance : Comparable Expression where
  obligations e := [Expression.toString e]
