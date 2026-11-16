import Father.Expression

inductive Effect where
  | updatesInstanceAttribute (attr : String) (expr : Expression) : Effect
  | conditionalOn (condition : Prop) : Effect → Effect

def exampleEffect := Effect.updatesInstanceAttribute "@written_conversation" exampleExpr
