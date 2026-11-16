inductive Expression where
  | functionCall (called : String)
  | fromDB (query : String) : Expression
  | unspecified (literal : String) : Expression
