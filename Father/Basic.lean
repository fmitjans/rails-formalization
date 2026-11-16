import Father.Comparable
import Father.Expression

inductive Module (α : Type) where
  | mk (name : String) (content : α) : Module α

inductive Controller (α : Type) where
  | mk (name : String) (content : α) : Controller α

inductive Code where
  | frozenStringDeclaration
  | fromComparable (header : String) (content : List Code) : Code
  | includeStatement : String → Code
  | before_action
    (callbacks : List String)
    (onlyOn : List String)
    (customCall : Option String)

inductive Effect where
  | updatesInstanceAttribute (attr : String) (expr : Expression) : Effect
  | conditionalOn (condition : Prop) : Effect → Effect

def exampleExpr := Expression.fromDB "conversations"
def exampleEffect := Effect.updatesInstanceAttribute "@written_conversation" exampleExpr

inductive FileDescription where
  | «partial» : Code → FileDescription
  | complete : Code → FileDescription

def String.toSymbol (name : String) := ":" ++ name

def before_actionComparable (callbacks : List String)
    (onlyOn : List String)
    (customCall : Option String) :=
    let callBacksString := callbacks.map (·.toSymbol) |> ", ".intercalate
    let onlyOnString := s!"only: %i[{" ".intercalate onlyOn}]"
    let argumentString := [callBacksString, onlyOnString].filter (not ·.isEmpty) |> ", ".intercalate
    customCall.elim
      s!"before_action {argumentString}"
      fun call => s!"before_action({argumentString}) \{ {call} }"

-- #eval ",".intercalate ["3", "4"]
example : before_actionComparable ["set_written_conversation"] ["show", "messages"] none
       = "before_action :set_written_conversation, only: %i[show messages]" := by decide
example : before_actionComparable ["set_transcript"] ["show"] none
       = "before_action :set_transcript, only: %i[show]" := by decide
example : before_actionComparable ["set_messages"] ["messages"] none
       = "before_action :set_messages, only: %i[messages]" := by decide
example : before_actionComparable [] ["show"] "load_conversation_opportunity(@written_conversation)"
       = "before_action(only: %i[show]) { load_conversation_opportunity(@written_conversation) }" := by decide

def Code.obligations : Code → List String
  | .frozenStringDeclaration => ["# frozen_string_literal: true"]
  | .fromComparable header content => [header] ++ content.flatMap Code.obligations ++ ["end"]
  | .includeStatement included => [s!"include {included}"]
  | .before_action callbacks onlyOn customCall => [before_actionComparable callbacks onlyOn customCall]

instance : Comparable Code where
  obligations := Code.obligations

instance [Comparable α] : Comparable (Module α) where
  obligations
    | .mk name content => [s!"module {name}"] ++ Comparable.obligations content ++ ["end"]

instance [Comparable α] : Comparable (Controller α) where
  obligations
    | .mk name content => [s!"class {name}Controller < ApplicationController"] ++ Comparable.obligations content ++ ["end"]

def Code.ofModule : Module (List Code) → Code
  | .mk name content => .fromComparable s!"module {name}" content

def Code.ofController : Controller (List Code) → Code
  | .mk name content => .fromComparable s!"class {name}Controller < ApplicationController" content

open Code
def writtenConversation : List Code := [
  frozenStringDeclaration,
  Code.ofModule (.mk "Api" [
    Code.ofController (.mk "WrittenConversations" [
      includeStatement "LoadsConversationOpportunity",
      before_action ["set_written_conversation"] ["show", "messages"] none
    ])
  ])
]
