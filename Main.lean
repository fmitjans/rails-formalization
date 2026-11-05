import Father

def comparableLines (path : System.FilePath) : IO (List String) := do
  let lines ← IO.FS.lines path
  pure <| lines.toList.filter (! ·.isEmpty) |> List.map (fun line => line.trimAscii.toString)

#print Add

def main : IO Unit := do
  let home ← IO.getEnv "HOME"
  let home := home.getD "/"
  let diioPath := System.mkFilePath [home, "Desktop", "diio", "Repos", "diio"]
  let controllersPath := System.mkFilePath ["app", "controllers"]
  let exampleFile := System.mkFilePath ["api", "written_conversations_controller.rb"]
  let completePath := (diioPath.join <| controllersPath.join exampleFile)
  let realLines ← comparableLines completePath
  let desiredLines := writtenConversation.comparables
  println! realLines.zip desiredLines
    |> List.filter (fun pair : String × String => pair.1 ≠ pair.2)
    |> List.map fun pair =>
      let firstDiffPos := String.firstDiffPos pair.1 pair.2 |> String.Pos.Raw.byteIdx
      (pair.1.drop firstDiffPos, pair.2.drop firstDiffPos)
