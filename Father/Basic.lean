import Father.Comparable

inductive Code where
  | frozenStringDeclaration : Code
  | moduleDeclaration : String → Code
  | controllerDeclaration : String → Code
  | includeStatement : String → Code
  | before_action
    (callbacks : List String)
    (onlyOn : List String)
    (customCall : Option String) : Code

inductive Expression where
  | fromDB (query : String) : Expression

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

instance : Comparable Code where
  obligations
    | .frozenStringDeclaration => ["# frozen_string_literal: true"]
    | .moduleDeclaration name => [s!"module {name}"]
    | .controllerDeclaration name => [s!"class {name}Controller < ApplicationController"]
    | .includeStatement included => [s!"include {included}"]
    | .before_action callbacks onlyOn customCall => [before_actionComparable callbacks onlyOn customCall]

structure Controller where
  innerPath : List String
  content : List Code

open Code
def writtenConversation : Controller := {
  innerPath := ["written_conversations"],
  content := open Code in [
    frozenStringDeclaration,
    moduleDeclaration "Api",
    controllerDeclaration "WrittenConversations",
    includeStatement "LoadsConversationOpportunity",
    before_action ["set_written_conversation"] ["show", "messages"] none
  ]
}

def Controller.comparables (controller : Controller) := controller.content.flatMap fun code => Comparable.obligations code
