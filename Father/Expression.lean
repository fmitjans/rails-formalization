import Father.Comparable

-- Expresions are lines that could potentially return something,
-- even if, like Code, they have side effects when evaluated

structure Identifier where
  name : String

inductive Expression where
  | functionCall (called : String) (arguments : List Expression) : Expression
  | fromDB (query : String) : Expression
  | unspecified (literal : String) : Expression
  | identifier (id : Identifier) : Expression
  | chainedCall (receiver : Expression) (chain : List Expression) : Expression
  | multilineChainedCall (receiver : Expression) (chain : List Expression) : Expression

open Expression
def exampleExpr := functionCall "authorize" [unspecified "@written_conversation"]

def Expression.toString : Expression → String
  | functionCall called arguments => s!"{called}({", ".intercalate (arguments.map Expression.toString)})"
  | fromDB query => query
  | unspecified literal => literal
  | identifier id => id.name
  | chainedCall receiver chain =>
    receiver.toString ++ "".intercalate (chain.map (s!".{Expression.toString ·}"))
  | multilineChainedCall receiver chain =>
    receiver.toString ++ "".intercalate (chain.map (s!".{Expression.toString ·}"))

instance : ToString Expression := ⟨Expression.toString⟩

def Expression.obligations : Expression → List String
  | multilineChainedCall receiver chain => receiver.toString :: chain.map (s!".{Expression.toString ·}")
  | e => [Expression.toString e]

instance : Comparable Expression where
  obligations := Expression.obligations
