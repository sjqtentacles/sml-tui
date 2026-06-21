(* test.sml - TDD suite for sml-tui. Written before the implementation. *)
structure Tests =
struct
  open Harness
  structure T = Tui

  (* Helper: the text content of a single row y in a buffer. *)
  fun rowText buf y =
    String.implode
      (List.tabulate (T.width buf, fn x => #ch (T.get buf (x, y))))

  fun runAll () =
  let
    (* ---------- colors / styles ---------- *)
    val () = section "colors and styles"
    val () = checkString "default style sgr is reset(0)" ("0", T.styleSgr T.defaultStyle)
    val () = checkString "fg red sgr" ("31", T.styleSgr (T.fg T.Red))
    val () = checkString "fg green sgr" ("32", T.styleSgr (T.fg T.Green))
    val () = checkString "bg blue on default fg" ("44", T.styleSgr (T.withBg T.Blue T.defaultStyle))
    val () = checkString "bold default" ("1", T.styleSgr (T.bold T.defaultStyle))
    val () = checkString "bright red fg" ("91", T.styleSgr (T.fg (T.Bright T.Red)))
    val () = checkString "color256 fg" ("38;5;200", T.styleSgr (T.fg (T.Color256 200)))
    (* combined: bold red on white *)
    val combo = T.bold (T.withBg T.White (T.fg T.Red))
    val () = checkString "bold red on white" ("1;31;47", T.styleSgr combo)

    (* ---------- buffer basics ---------- *)
    val () = section "buffer basics"
    val b0 = T.make (5, 3)
    val () = checkInt "width" (5, T.width b0)
    val () = checkInt "height" (3, T.height b0)
    val () = checkBool "fresh cell is space" (true, #ch (T.get b0 (0,0)) = #" ")
    val () = checkBool "out of bounds is blank space" (true, #ch (T.get b0 (99,99)) = #" ")
    val () = checkBool "negative coord is blank space" (true, #ch (T.get b0 (~1,0)) = #" ")

    (* setChar is non-destructive on the original *)
    val b1 = T.setChar b0 (1,1) #"X" (T.fg T.Red)
    val () = checkBool "setChar writes the cell" (true, #ch (T.get b1 (1,1)) = #"X")
    val () = checkBool "setChar preserves fg" (true, #fg (#style (T.get b1 (1,1))) = T.Red)
    val () = checkBool "original buffer unchanged" (true, #ch (T.get b0 (1,1)) = #" ")
    val () = checkBool "setChar out of bounds is a no-op" (true,
               #ch (T.get (T.setChar b0 (99,99) #"Z" T.defaultStyle) (0,0)) = #" ")

    (* ---------- drawText ---------- *)
    val () = section "drawText"
    val bt = T.drawText (T.make (10,1)) (0,0) T.defaultStyle "hello"
    val () = checkString "drawText basic" ("hello     ", rowText bt 0)
    val bt2 = T.drawText (T.make (4,1)) (0,0) T.defaultStyle "hello"
    val () = checkString "drawText clips at right edge" ("hell", rowText bt2 0)
    val bt3 = T.drawText (T.make (6,1)) (2,0) T.defaultStyle "hi"
    val () = checkString "drawText with x offset" ("  hi  ", rowText bt3 0)
    (* negative x: characters before column 0 are clipped *)
    val bt4 = T.drawText (T.make (4,1)) (~2,0) T.defaultStyle "test"
    val () = checkString "drawText clips negative start" ("st  ", rowText bt4 0)

    (* ---------- fillRect / drawBox ---------- *)
    val () = section "fillRect and drawBox"
    val bf = T.fillRect (T.make (5,3)) {x=1,y=1,w=2,h=1} #"#" T.defaultStyle
    val () = checkString "fillRect row0 untouched" ("     ", rowText bf 0)
    val () = checkString "fillRect row1 filled" (" ##  ", rowText bf 1)
    val () = checkString "fillRect row2 untouched" ("     ", rowText bf 2)

    val bb = T.drawBox (T.make (4,3)) {x=0,y=0,w=4,h=3} T.defaultStyle
    (* Box corners use ASCII line-drawing; verify structurally:
       top-left corner present, interior empty, left edge present. *)
    val () = checkBool "box top-left corner present" (true,
               #ch (T.get bb (0,0)) <> #" ")
    val () = checkBool "box interior is empty" (true, #ch (T.get bb (1,1)) = #" ")
    val () = checkBool "box left edge present" (true, #ch (T.get bb (0,1)) <> #" ")

    (* ---------- toText ---------- *)
    val () = section "toText"
    val tb = T.drawText (T.make (3,2)) (0,0) T.defaultStyle "ab"
    val () = checkString "toText joins rows with newline" ("ab \n   ", T.toText tb)

    (* ---------- toAnsi ---------- *)
    val () = section "toAnsi"
    val ansi = T.toAnsi (T.make (1,1))
    val () = checkBool "ansi starts with clear+home" (true,
               String.isPrefix "\027[2J\027[H" ansi)
    val () = checkBool "ansi ends with reset" (true,
               String.isSuffix "\027[0m" ansi)

    (* ---------- measure ---------- *)
    val () = section "measure"
    val () = checkBool "measure Text" (true, T.measure (T.Text (T.defaultStyle, "hello")) = (5,1))
    val () = checkBool "measure Lines uses longest" (true,
               T.measure (T.Lines (T.defaultStyle, ["ab","abcd","a"])) = (4,3))
    val () = checkBool "measure VBox sums heights, max width" (true,
               T.measure (T.VBox [T.Text (T.defaultStyle,"ab"), T.Text (T.defaultStyle,"abcd")]) = (4,2))
    val () = checkBool "measure HBox sums widths, max height" (true,
               T.measure (T.HBox [T.Text (T.defaultStyle,"ab"), T.Lines (T.defaultStyle,["x","y"])]) = (3,2))
    val () = checkBool "measure Border adds 2 each dim" (true,
               T.measure (T.Border (T.Text (T.defaultStyle,"hi"))) = (4,3))
    val () = checkBool "measure Pad adds 2*n each dim" (true,
               T.measure (T.Pad (1, T.Text (T.defaultStyle,"hi"))) = (4,3))
    val () = checkBool "measure Fixed is its own size" (true,
               T.measure (T.Fixed (10, 4, T.Text (T.defaultStyle,"hi"))) = (10,4))

    (* ---------- render / draw ---------- *)
    val () = section "render and draw"
    val dText = T.draw (T.Text (T.defaultStyle, "hi"))
    val () = checkString "draw Text content" ("hi", rowText dText 0)
    val () = checkInt "draw Text width" (2, T.width dText)

    val dV = T.draw (T.VBox [T.Text (T.defaultStyle,"ab"), T.Text (T.defaultStyle,"cd")])
    val () = checkString "vbox row0" ("ab", rowText dV 0)
    val () = checkString "vbox row1" ("cd", rowText dV 1)

    val dH = T.draw (T.HBox [T.Text (T.defaultStyle,"ab"), T.Text (T.defaultStyle,"cd")])
    val () = checkString "hbox single row" ("abcd", rowText dH 0)

    val dBorder = T.draw (T.Border (T.Text (T.defaultStyle, "hi")))
    val () = checkInt "border width" (4, T.width dBorder)
    val () = checkInt "border height" (3, T.height dBorder)
    val () = checkString "border center has content" ("|hi|", rowText dBorder 1)
    val () = checkBool "border has corner" (true, #ch (T.get dBorder (0,0)) <> #" ")

    val dPad = T.draw (T.Pad (1, T.Text (T.defaultStyle, "x")))
    val () = checkInt "pad width" (3, T.width dPad)
    val () = checkString "pad center row" (" x ", rowText dPad 1)

    (* ---------- parseKey ---------- *)
    val () = section "parseKey"
    fun key s = T.parseKey s
    val () = checkBool "plain char" (true, key "a" = (T.Char #"a", 1))
    val () = checkBool "enter (\\r)" (true, key "\r" = (T.Enter, 1))
    val () = checkBool "enter (\\n)" (true, key "\n" = (T.Enter, 1))
    val () = checkBool "tab" (true, key "\t" = (T.Tab, 1))
    val () = checkBool "space" (true, key " " = (T.Space, 1))
    val () = checkBool "backspace (127)" (true, key "\127" = (T.Backspace, 1))
    val () = checkBool "lone escape" (true, key "\027" = (T.Escape, 1))
    val () = checkBool "arrow up" (true, key "\027[A" = (T.Up, 3))
    val () = checkBool "arrow down" (true, key "\027[B" = (T.Down, 3))
    val () = checkBool "arrow right" (true, key "\027[C" = (T.Right, 3))
    val () = checkBool "arrow left" (true, key "\027[D" = (T.Left, 3))
    val () = checkBool "home" (true, key "\027[H" = (T.Home, 3))
    val () = checkBool "end" (true, key "\027[F" = (T.End, 3))
    (* consuming only the leading key from a longer buffer *)
    val () = checkBool "consume one of many" (true, key "abc" = (T.Char #"a", 1))
    val () = checkBool "arrow then char consumes 3" (true, #2 (key "\027[Az") = 3)

    (* ---------- Elm runtime: step / frame ---------- *)
    val () = section "Elm runtime"
    (* a counter app: model is int; +/- adjust; q quits *)
    datatype msg = Inc | Dec
    val counter : (int, msg) T.app =
      { init = 0
      , update = (fn Inc => (fn n => n + 1) | Dec => (fn n => n - 1))
      , view = (fn n => T.Text (T.defaultStyle, "Count: " ^ Int.toString n))
      , onKey = (fn (T.Char #"+") => SOME Inc
                  | (T.Up)         => SOME Inc
                  | (T.Char #"-")  => SOME Dec
                  | (T.Down)       => SOME Dec
                  | _              => NONE)
      , quit = (fn _ => false) }

    val () = checkInt "step inc via +" (1, T.step counter (T.KeyEvent (T.Char #"+")) 0)
    val () = checkInt "step inc via Up" (6, T.step counter (T.KeyEvent T.Up) 5)
    val () = checkInt "step dec via -" (2, T.step counter (T.KeyEvent (T.Char #"-")) 3)
    val () = checkInt "step unmapped key is no-op" (3, T.step counter (T.KeyEvent (T.Char #"z")) 3)
    val () = checkInt "step Tick is no-op" (7, T.step counter T.Tick 7)
    val () = checkInt "step Resize is no-op" (7, T.step counter (T.Resize (80,24)) 7)
    (* chained *)
    val n = List.foldl (fn (e,m) => T.step counter e m) 0
              [T.KeyEvent (T.Char #"+"), T.KeyEvent T.Up, T.KeyEvent (T.Char #"+"),
               T.KeyEvent (T.Char #"-")]
    val () = checkInt "chained steps" (2, n)

    val fr = T.frame counter 42
    val () = checkBool "frame is ansi (clear+home)" (true, String.isPrefix "\027[2J\027[H" fr)
    (* toAnsi interleaves SGR codes per cell, so assert content via the pure
       plain-text render of the same view. *)
    val () = checkString "view renders the model text"
               ("Count: 42", T.toText (T.draw (#view counter 42)))
  in
    ()
  end

  fun run () = (reset (); runAll (); Harness.run ())
end
