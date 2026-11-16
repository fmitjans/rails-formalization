import Father.Comparable
import Father.Expression

inductive Code where
  | frozenStringDeclaration
  | includeStatement : String → Code
  | before_action
    (callbacks : List String)
    (onlyOn : List String)
    (customCall : Option String)

inductive Controller where
  | mk (name : String) (content : List Code) : Controller

structure Content where
  lines : List String

instance : Comparable Content where
  obligations c := c.lines

structure Module extends Content where
  name : String

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

def wrapperObligations [Comparable α] (header : String) (content : List α) : List String :=
  [header] ++ content.flatMap Comparable.obligations ++ ["end"]

def Code.obligations : Code → List String
  | .frozenStringDeclaration => ["# frozen_string_literal: true"]
  | .includeStatement included => [s!"include {included}"]
  | .before_action callbacks onlyOn customCall => [before_actionComparable callbacks onlyOn customCall]

instance : Comparable Code where
  obligations := Code.obligations

instance : Comparable Controller where
  obligations | .mk name content => wrapperObligations s!"class {name}Controller < ApplicationController" content

def Content.ofCode (content : List Code) : Content :=
  ⟨Comparable.obligations content⟩

def Content.ofController (controller : Controller) : Content :=
  ⟨Comparable.obligations controller⟩

def Module.mk2 [Comparable α] (name : String) (a : α) : Module := ⟨⟨Comparable.obligations a⟩, name⟩

instance : Comparable Module where
  obligations m := wrapperObligations s!"module {m.name}" m.lines

def Content.ofModule (module : Module) : Content :=
  ⟨Comparable.obligations module⟩

open Code
def writtenConversation : List Content := [
  .ofCode [frozenStringDeclaration],
  .ofModule ( .mk2 "Api"
    (Controller.mk "WrittenConversations" [
      includeStatement "LoadsConversationOpportunity",
      before_action ["set_written_conversation"] ["show", "messages"] none
    ])
  )
]
