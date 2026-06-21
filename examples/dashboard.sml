(* sml-tui demo: builds a styled widget tree, lays it out into a Tui.buffer
   (the same pure pipeline that feeds toAnsi for a real terminal), then
   rasterizes every cell - background block + glyph in its fg color - to
   assets/dashboard.png using the bitmap-font renderer. *)

open Tui

(* ----- a small dashboard UI ----- *)
val title = Text (bold (fg (Bright Cyan)), "sml-tui  -  pure terminal UI toolkit")

val left =
  Border (Pad (1, VBox
    [ Text (bold (fg Yellow), "Build")
    , Text (fg Green,  "  mlton    ok   88/0")
    , Text (fg Green,  "  poly ml  ok   88/0")
    , Text (fg (Bright Black), "  ----------------")
    , Text (fg Cyan,   "  coverage 92 percent")
    , Text (fg Red,    "  lint     2 warnings") ]))

val right =
  Border (Pad (1, VBox
    [ Text (bold (fg Magenta), "Tasks")
    , Text (fg White,  "  [x] parse buffer")
    , Text (fg White,  "  [x] layout engine")
    , Text (fg (Bright Green), "  [>] ansi renderer")
    , Text (fg (Bright Black), "  [ ] mouse events") ]))

val footer =
  Lines (fg (Bright Black),
    [ "keys: arrows move   enter select   q quit"
    , "rendered purely: widget to buffer to pixels" ])

val ui =
  Border (Pad (1, VBox [ title
                       , Text (defaultStyle, "")
                       , HBox [ left, Text (defaultStyle, " "), right ]
                       , Text (defaultStyle, "")
                       , footer ]))

val buf = draw ui

(* ----- ANSI color -> RGB ----- *)
fun clamp v = if v < 0 then 0 else if v > 255 then 255 else v
fun bright (r, g, b) = (clamp (r + 70), clamp (g + 70), clamp (b + 70))

fun baseRgb c =
  case c of
      Black   => (44, 47, 54)
    | Red     => (201, 86, 82)
    | Green   => (124, 186, 108)
    | Yellow  => (204, 180, 84)
    | Blue    => (92, 140, 224)
    | Magenta => (186, 108, 196)
    | Cyan    => (96, 178, 196)
    | White   => (198, 201, 208)
    | Default => (210, 214, 222)
    | Bright c => bright (baseRgb c)
    | Color256 n =>
        if n < 16 then baseRgb White
        else if n < 232 then
          let
            val m = n - 16
            fun lvl v = if v = 0 then 0 else 55 + 40 * v
          in
            (lvl (m div 36), lvl ((m div 6) mod 6), lvl (m mod 6))
          end
        else let val v = 8 + (n - 232) * 10 in (v, v, v) end

val termBg = (22, 24, 30)
fun bgRgb Default = termBg | bgRgb c = baseRgb c

(* ----- rasterize the buffer ----- *)
val scale = 2
val cellW = 6 * scale
val cellH = 10 * scale
val margin = 10
val cols = width buf
val rows = height buf
val imgW = cols * cellW + 2 * margin
val imgH = rows * cellH + 2 * margin

val c = Canvas.make (imgW, imgH) termBg

val () =
  let
    fun cellAt (x, y) =
      let
        val { ch, style } = get buf (x, y)
        val { fg = f, bg = b, reverse, ... } = style
        val (fgc, bgc) = if reverse then (bgRgb b, baseRgb f) else (baseRgb f, bgRgb b)
        val px = margin + x * cellW
        val py = margin + y * cellH
      in
        Canvas.fillRect c (px, py, cellW, cellH) bgc;
        if ch <> #" " then Font.drawChar c (px, py + scale) scale fgc ch else ()
      end
    fun loopRow y =
      if y >= rows then ()
      else
        let
          fun loopCol x = if x >= cols then () else (cellAt (x, y); loopCol (x + 1))
        in
          loopCol 0; loopRow (y + 1)
        end
  in
    loopRow 0
  end

val () =
  let
    val os = BinIO.openOut "assets/dashboard.png"
  in
    BinIO.output (os, Image.encodePng (Canvas.toImage c));
    BinIO.closeOut os;
    print "wrote assets/dashboard.png\n"
  end
