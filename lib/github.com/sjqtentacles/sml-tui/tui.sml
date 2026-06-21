(* tui.sml - implementation of the pure terminal UI toolkit.
 *
 * The buffer is a flat char/style array indexed row-major. All public drawing
 * functions copy-on-write so values are persistent. The only impure function is
 * App.run, which is isolated at the bottom of the file. *)
structure Tui :> TUI =
struct
  (* ---------------- colors / styles ---------------- *)
  datatype color =
      Default
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    | Bright of color
    | Color256 of int

  type style = { fg : color, bg : color, bold : bool, underline : bool, reverse : bool }

  val defaultStyle : style =
    { fg = Default, bg = Default, bold = false, underline = false, reverse = false }

  fun fg c = { fg = c, bg = Default, bold = false, underline = false, reverse = false }
  fun withBg c (s:style) =
    { fg = #fg s, bg = c, bold = #bold s, underline = #underline s, reverse = #reverse s }
  fun bold (s:style) =
    { fg = #fg s, bg = #bg s, bold = true, underline = #underline s, reverse = #reverse s }

  (* base code: foreground offset 30, background offset 40 *)
  fun baseCode Black = SOME 0 | baseCode Red = SOME 1 | baseCode Green = SOME 2
    | baseCode Yellow = SOME 3 | baseCode Blue = SOME 4 | baseCode Magenta = SOME 5
    | baseCode Cyan = SOME 6 | baseCode White = SOME 7 | baseCode _ = NONE

  (* Produce SGR parameter fragments for a foreground color. *)
  fun fgParams Default = []
    | fgParams (Color256 n) = ["38;5;" ^ Int.toString n]
    | fgParams (Bright c) =
        (case baseCode c of SOME n => [Int.toString (90 + n)] | NONE => [])
    | fgParams c = (case baseCode c of SOME n => [Int.toString (30 + n)] | NONE => [])

  fun bgParams Default = []
    | bgParams (Color256 n) = ["48;5;" ^ Int.toString n]
    | bgParams (Bright c) =
        (case baseCode c of SOME n => [Int.toString (100 + n)] | NONE => [])
    | bgParams c = (case baseCode c of SOME n => [Int.toString (40 + n)] | NONE => [])

  fun styleSgr (s:style) =
    let
      val parts =
          (if #bold s then ["1"] else [])
        @ (if #underline s then ["4"] else [])
        @ (if #reverse s then ["7"] else [])
        @ fgParams (#fg s)
        @ bgParams (#bg s)
    in
      case parts of [] => "0" | _ => String.concatWith ";" parts
    end

  (* ---------------- buffer ---------------- *)
  type cell = { ch : char, style : style }

  (* width, height, and a row-major array of cells *)
  type buffer = { w : int, h : int, cells : cell vector }

  val blank : cell = { ch = #" ", style = defaultStyle }

  fun make (w, h) =
    let val w = Int.max (0, w) and h = Int.max (0, h)
    in { w = w, h = h, cells = Vector.tabulate (w * h, fn _ => blank) } end

  fun width  (b:buffer) = #w b
  fun height (b:buffer) = #h b

  fun inBounds (b:buffer) (x, y) = x >= 0 andalso y >= 0 andalso x < #w b andalso y < #h b

  fun get (b:buffer) (x, y) =
    if inBounds b (x, y) then Vector.sub (#cells b, y * #w b + x) else blank

  fun setChar (b:buffer) (x, y) ch style =
    if not (inBounds b (x, y)) then b
    else
      let val idx = y * #w b + x
          val cells' = Vector.update (#cells b, idx, { ch = ch, style = style })
      in { w = #w b, h = #h b, cells = cells' } end

  fun drawText b (x, y) style s =
    let
      val cs = String.explode s
      fun go _ [] acc = acc
        | go i (c::rest) acc = go (i+1) rest (setChar acc (x+i, y) c style)
    in go 0 cs b end

  fun fillRect b {x, y, w, h} ch style =
    let
      fun rows yy acc =
        if yy >= y + h then acc
        else
          let
            fun cols xx acc =
              if xx >= x + w then acc
              else cols (xx+1) (setChar acc (xx, yy) ch style)
          in rows (yy+1) (cols x acc) end
    in rows y b end

  (* ASCII single-line box characters (one byte each so they fit in a cell). *)
  fun drawBox b {x, y, w, h} style =
    if w <= 0 orelse h <= 0 then b
    else
      let
        val x2 = x + w - 1
        val y2 = y + h - 1
        (* corners *)
        val b = setChar b (x, y)   #"+" style
        val b = setChar b (x2, y)  #"+" style
        val b = setChar b (x, y2)  #"+" style
        val b = setChar b (x2, y2) #"+" style
        (* horizontal edges *)
        fun hline yy acc =
          let fun go xx acc = if xx >= x2 then acc else go (xx+1) (setChar acc (xx, yy) #"-" style)
          in go (x+1) acc end
        val b = hline y b
        val b = hline y2 b
        (* vertical edges *)
        fun vline xx acc =
          let fun go yy acc = if yy >= y2 then acc else go (yy+1) (setChar acc (xx, yy) #"|" style)
          in go (y+1) acc end
        val b = vline x b
        val b = vline x2 b
      in b end

  fun toText (b:buffer) =
    let
      fun row y = String.implode (List.tabulate (#w b, fn x => #ch (get b (x, y))))
    in String.concatWith "\n" (List.tabulate (#h b, row)) end

  fun toAnsi (b:buffer) =
    let
      fun cellStr (c:cell) = "\027[" ^ styleSgr (#style c) ^ "m" ^ String.str (#ch c)
      fun row y =
        String.concat (List.tabulate (#w b, fn x => cellStr (get b (x, y)))) ^ "\027[0m"
      val body = String.concatWith "\n" (List.tabulate (#h b, row))
    in "\027[2J\027[H" ^ body ^ "\027[0m" end

  (* ---------------- widgets ---------------- *)
  datatype widget =
      Text of style * string
    | Lines of style * string list
    | VBox of widget list
    | HBox of widget list
    | Border of widget
    | Pad of int * widget
    | Fixed of int * int * widget

  fun measure (Text (_, s)) = (String.size s, 1)
    | measure (Lines (_, ls)) =
        let val w = List.foldl (fn (l, m) => Int.max (m, String.size l)) 0 ls
        in (w, List.length ls) end
    | measure (VBox ws) =
        List.foldl (fn (w, (mw, mh)) =>
          let val (cw, ch) = measure w in (Int.max (mw, cw), mh + ch) end) (0, 0) ws
    | measure (HBox ws) =
        List.foldl (fn (w, (mw, mh)) =>
          let val (cw, ch) = measure w in (mw + cw, Int.max (mh, ch)) end) (0, 0) ws
    | measure (Border w) =
        let val (cw, ch) = measure w in (cw + 2, ch + 2) end
    | measure (Pad (n, w)) =
        let val (cw, ch) = measure w in (cw + 2*n, ch + 2*n) end
    | measure (Fixed (w, h, _)) = (w, h)

  fun render b (x, y) (Text (st, s)) = drawText b (x, y) st s
    | render b (x, y) (Lines (st, ls)) =
        let
          fun go _ [] acc = acc
            | go i (l::rest) acc = go (i+1) rest (drawText acc (x, y+i) st l)
        in go 0 ls b end
    | render b (x, y) (VBox ws) =
        let
          fun go _ [] acc = acc
            | go yy (w::rest) acc =
                let val (_, ch) = measure w
                in go (yy + ch) rest (render acc (x, yy) w) end
        in go y ws b end
    | render b (x, y) (HBox ws) =
        let
          fun go _ [] acc = acc
            | go xx (w::rest) acc =
                let val (cw, _) = measure w
                in go (xx + cw) rest (render acc (xx, y) w) end
        in go x ws b end
    | render b (x, y) (Border w) =
        let
          val (cw, ch) = measure w
          val b = drawBox b {x = x, y = y, w = cw + 2, h = ch + 2} defaultStyle
        in render b (x+1, y+1) w end
    | render b (x, y) (Pad (n, w)) = render b (x+n, y+n) w
    | render b (x, y) (Fixed (_, _, w)) = render b (x, y) w

  fun draw w =
    let val (cw, ch) = measure w
    in render (make (cw, ch)) (0, 0) w end

  (* ---------------- input events ---------------- *)
  datatype key =
      Char of char
    | Enter | Escape | Tab | Backspace | Space
    | Up | Down | Left | Right
    | Home | End
    | FKey of int
    | Unknown of string

  datatype event = KeyEvent of key | Resize of int * int | Tick

  (* Parse the leading key. Handles single bytes and CSI escape sequences. *)
  fun parseKey s =
    if String.size s = 0 then (Unknown "", 0)
    else
      let val c0 = String.sub (s, 0) in
        case c0 of
            #"\r" => (Enter, 1)
          | #"\n" => (Enter, 1)
          | #"\t" => (Tab, 1)
          | #" "  => (Space, 1)
          | #"\127" => (Backspace, 1)
          | #"\008" => (Backspace, 1)
          | #"\027" =>
              (* escape: maybe a CSI/SS3 sequence ESC [ X  or  ESC O X *)
              if String.size s >= 3
                 andalso (String.sub (s,1) = #"[" orelse String.sub (s,1) = #"O")
              then
                let val f = String.sub (s, 2) in
                  case f of
                      #"A" => (Up, 3)
                    | #"B" => (Down, 3)
                    | #"C" => (Right, 3)
                    | #"D" => (Left, 3)
                    | #"H" => (Home, 3)
                    | #"F" => (End, 3)
                    | _    => (Unknown (String.substring (s, 0, 3)), 3)
                end
              else (Escape, 1)
          | _ => (Char c0, 1)
      end

  (* ---------------- Elm runtime ---------------- *)
  type ('model, 'msg) app =
    { init   : 'model
    , update : 'msg -> 'model -> 'model
    , view   : 'model -> widget
    , onKey  : key -> 'msg option
    , quit   : 'model -> bool }

  fun step (app:('model,'msg) app) ev model =
    case ev of
        KeyEvent k =>
          (case (#onKey app) k of
               SOME msg => (#update app) msg model
             | NONE => model)
      | _ => model

  fun frame (app:('model,'msg) app) model = toAnsi (draw ((#view app) model))

  (* ----- impure terminal loop (isolated) ----- *)
  (* Put the terminal in raw, no-echo mode, hide the cursor, render frames, and
   * read one keystroke at a time, dispatching through the pure `step`. Restores
   * the terminal on exit. Uses stty via OS.Process for portability. *)
  fun run (app:('model,'msg) app) =
    let
      fun sh cmd = ignore (OS.Process.system cmd)
      val () = sh "stty -echo -icanon min 1 time 0 < /dev/tty"
      val () = print "\027[?25l"   (* hide cursor *)

      fun restore () =
        (print "\027[?25h";        (* show cursor *)
         print "\027[0m";
         sh "stty sane < /dev/tty")

      fun readKey () =
        (* read up to 3 bytes so escape sequences arrive together *)
        let val first = TextIO.input1 TextIO.stdIn in
          case first of
              NONE => (Escape, 0)
            | SOME c =>
                if c = #"\027" then
                  let
                    val rest = TextIO.canInput (TextIO.stdIn, 2)
                    val more = case rest of
                                   SOME n => if n > 0 then TextIO.inputN (TextIO.stdIn, n) else ""
                                 | NONE => ""
                    val (k, _) = parseKey (String.str c ^ more)
                  in (k, 0) end
                else (#1 (parseKey (String.str c)), 0)
        end

      fun loop model =
        if (#quit app) model then model
        else
          (print (frame app model);
           let val (k, _) = readKey ()
           in loop (step app (KeyEvent k) model) end)

      val result = loop (#init app) handle e => (restore (); raise e)
      val () = restore ()
    in result end
end
