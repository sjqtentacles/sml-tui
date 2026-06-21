(* tui.sig - pure terminal UI toolkit for Standard ML
 *
 * Everything in this signature is pure and deterministic except the single
 * `App.run` entry point, which owns the impure terminal event loop. The screen
 * is modelled as an immutable grid of cells; widgets describe a layout that is
 * laid out into a region of that grid; the grid is serialized to an ANSI string.
 * Because the whole pipeline (widget -> buffer -> ANSI string) is pure, every
 * frame can be snapshot-tested without a real terminal. *)
signature TUI =
sig
  (* ----- Colors ----- *)
  (* The 8 standard ANSI colors plus Default (terminal's own fg/bg) and an
   * arbitrary 256-color palette index. *)
  datatype color =
      Default
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    | Bright of color           (* the bright/bold variant of a base color *)
    | Color256 of int           (* 0..255 of the xterm 256-color palette *)

  (* A text style: foreground, background, and bold/underline/reverse flags. *)
  type style = { fg : color, bg : color, bold : bool, underline : bool, reverse : bool }
  val defaultStyle : style
  val fg : color -> style                 (* default style with this fg *)
  val withBg : color -> style -> style
  val bold : style -> style

  (* SGR (Select Graphic Rendition) parameter string for a style, e.g. the
   * digits between ESC[ and m. Exposed for testing the ANSI layer. *)
  val styleSgr : style -> string

  (* ----- Screen buffer ----- *)
  (* An immutable grid of cells. Coordinates are 0-based (col x, row y) with the
   * origin at the top-left. Drawing outside the bounds is silently clipped. *)
  type cell = { ch : char, style : style }
  type buffer

  val make    : int * int -> buffer       (* (width, height), filled with spaces *)
  val width   : buffer -> int
  val height  : buffer -> int
  val get     : buffer -> int * int -> cell      (* (x,y); out of bounds -> blank cell *)

  (* Drawing primitives. All return a new buffer; the input is unchanged. *)
  val setChar : buffer -> int * int -> char -> style -> buffer
  val drawText : buffer -> int * int -> style -> string -> buffer   (* left-to-right from (x,y) *)
  val fillRect : buffer -> { x:int, y:int, w:int, h:int } -> char -> style -> buffer
  val drawBox  : buffer -> { x:int, y:int, w:int, h:int } -> style -> buffer  (* single-line box border *)

  (* Render the whole buffer to a plain string of rows joined by '\n', ignoring
   * style (useful for snapshot tests of layout). *)
  val toText : buffer -> string

  (* Render the buffer to a full ANSI escape string: a leading clear+home, then
   * each row with SGR styling and a trailing reset. Deterministic. *)
  val toAnsi : buffer -> string

  (* ----- Widgets / layout ----- *)
  (* A declarative widget tree. `layout` paints a widget into a buffer at a
   * region. Sizes are content-driven; vbox stacks vertically, hbox side by side. *)
  datatype widget =
      Text of style * string
    | Lines of style * string list
    | VBox of widget list                 (* stack children top to bottom *)
    | HBox of widget list                 (* place children left to right *)
    | Border of widget                    (* single-line box around child *)
    | Pad of int * widget                 (* n spaces of padding on all sides *)
    | Fixed of int * int * widget         (* clamp child into w x h region *)

  (* Natural (content) size of a widget, as (width, height). *)
  val measure : widget -> int * int

  (* Paint a widget into buffer starting at (x,y). Returns the new buffer. *)
  val render : buffer -> int * int -> widget -> buffer

  (* Convenience: measure the widget, allocate a buffer of that size, render. *)
  val draw : widget -> buffer

  (* ----- Input events ----- *)
  datatype key =
      Char of char
    | Enter | Escape | Tab | Backspace | Space
    | Up | Down | Left | Right
    | Home | End
    | FKey of int                         (* F1..F12 *)
    | Unknown of string

  datatype event = KeyEvent of key | Resize of int * int | Tick

  (* Parse a single key from the leading bytes of a string (as read from a raw
   * terminal). Returns the key and the number of bytes consumed. Handles ANSI
   * escape sequences like "\027[A" (Up). Pure and total. *)
  val parseKey : string -> key * int

  (* ----- Elm-style application runtime ----- *)
  (* An app is defined purely by (init model, update, view). `step` advances the
   * model by one event purely and is fully testable. `run` is the only impure
   * piece: it puts the terminal in raw mode, renders frames, and pumps events. *)
  type ('model, 'msg) app =
    { init   : 'model
    , update : 'msg -> 'model -> 'model
    , view   : 'model -> widget
    , onKey  : key -> 'msg option          (* map a key to a message, NONE = ignore *)
    , quit   : 'model -> bool }             (* when true, the loop exits *)

  (* Purely apply one event to a model, returning the new model. A key that maps
   * to NONE (or a non-key event) leaves the model unchanged. *)
  val step : ('model, 'msg) app -> event -> 'model -> 'model

  (* Render an app's current model to its ANSI frame string (pure). *)
  val frame : ('model, 'msg) app -> 'model -> string

  (* Run the interactive loop on the real terminal. Impure. Returns the final
   * model when `quit` becomes true. *)
  val run : ('model, 'msg) app -> 'model
end
