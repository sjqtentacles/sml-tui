(* counter.sml - the classic Elm-architecture counter, in the terminal.
 *
 * Run with:  make example && ./bin/counter
 * Keys:  + / = / Up to increment,  - / Down to decrement,  r reset,  q / Esc quit.
 *
 * Everything here is pure (init/update/view/onKey/quit). Tui.run owns the only
 * impure terminal interaction.
 *
 * The model carries a `running` flag; `quit` simply reports `not running`. This
 * is the idiomatic Elm way to let a keystroke end the program while keeping the
 * whole update/view/quit pipeline pure and snapshot-testable. *)

type model = { count : int, running : bool }

datatype msg = Inc | Dec | Reset | Quit

val update : msg -> model -> model =
  fn Inc   => (fn m => { count = #count m + 1, running = #running m })
   | Dec   => (fn m => { count = #count m - 1, running = #running m })
   | Reset => (fn m => { count = 0,            running = #running m })
   | Quit  => (fn m => { count = #count m,     running = false })

fun view (m : model) =
  let
    open Tui
    val n = #count m
    val title = Text (bold (fg Cyan), "sml-tui counter")
    val value = Text (bold (fg (if n < 0 then Red else Green)),
                      "Count: " ^ Int.toString n)
    val help  = Lines (fg (Bright Black),
                       [ "[+ / =/ up]  increment"
                       , "[- / down]   decrement"
                       , "[r]          reset"
                       , "[q / esc]    quit" ])
  in
    Border (Pad (1, VBox [ title
                         , Text (defaultStyle, "")
                         , value
                         , Text (defaultStyle, "")
                         , help ]))
  end

val onKey : Tui.key -> msg option =
  fn Tui.Char #"+" => SOME Inc
   | Tui.Char #"=" => SOME Inc
   | Tui.Up        => SOME Inc
   | Tui.Char #"-" => SOME Dec
   | Tui.Down      => SOME Dec
   | Tui.Char #"r" => SOME Reset
   | Tui.Char #"q" => SOME Quit
   | Tui.Escape    => SOME Quit
   | _             => NONE

val app : (model, msg) Tui.app =
  { init   = { count = 0, running = true }
  , update = update
  , view   = view
  , onKey  = onKey
  , quit   = (fn m => not (#running m)) }

fun main () = (ignore (Tui.run app); print "\nbye!\n")

val () = main ()
