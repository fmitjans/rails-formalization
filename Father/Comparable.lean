inductive ComparisonResult where
  | incorrect (real description : String)
  | partialMatch (missing : List String)
  | fullMatch
deriving Repr

class Comparable (α : Type) where
  obligations : α → List String

instance : Comparable String where obligations string := [string]

instance [Comparable α] : Comparable (List α) where
  obligations list := list.flatMap Comparable.obligations

private def firstDiffBytePos (realLine descriptionLine : String) : Nat :=
  String.firstDiffPos realLine descriptionLine |> String.Pos.Raw.byteIdx

private def diffSuffixes (realLine descriptionLine : String) : String × String :=
  let firstDiffPos := firstDiffBytePos realLine descriptionLine
  ((realLine.drop firstDiffPos).toString, (descriptionLine.drop firstDiffPos).toString)

def compare [Comparable α] (real : List String) (description : α) : ComparisonResult :=
  match real, (Comparable.obligations description) with
  | [], [] => .fullMatch
  | [], descriptionLine :: _ => .incorrect "" descriptionLine
  | realLine :: realRest, [] => .partialMatch (realLine :: realRest)
  | realLine :: realRest, descriptionLine :: descriptionRest =>
    if realLine = descriptionLine then
      compare realRest descriptionRest
    else
      let (realSuffix, descriptionSuffix) := diffSuffixes realLine descriptionLine
      .incorrect realSuffix descriptionSuffix
