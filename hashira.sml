signature HASHIRA =
sig
  type command
  type color

  type coord
  type position
  type field
  type state
  type cluster
  type mode

  val height : coord
  val width : coord

  val tick_ms : int
  val drop_frames : int

  val show_command : command -> string
  
  val rnd_col : Lcg.t -> Lcg.t * color
  val new_blocks : coord -> color * color * color -> cluster
  val drop : field -> cluster -> cluster
  val rot : color * color * color -> color * color * color
  val mark_clusters : field -> (coord * coord) list

  val handle_event : Sdl.event_type -> command option -> command option
  val apply_ev : field -> command -> cluster -> cluster
  val tick : mode -> command option -> field -> state -> bool -> mode * field * state

  val render : Sdl.renderer -> field -> mode -> (coord * coord) list option -> unit

  val init : unit -> Sdl.renderer * state
  val main : unit -> unit
end

structure Hashira : HASHIRA =
struct

  (** types and constants **)
  datatype command = Left | Right | Rotate | Drop | Quit
  datatype color = Red | Green | Blue | Yellow | Purple | Brown | Black | Background

  type coord = int
  type position = coord * coord
  type field = (coord * color) list vector
  type state = Lcg.t
  type cluster = position * (color * color * color)

  datatype mode = Falling of cluster | Landed of position

  val height = 12
  val width = 10
  val size = 40

  (** general utilities **)
  fun show_command Left = "Left"
    | show_command Right = "Right"
    | show_command Rotate = "Rotate"
    | show_command Drop = "Drop"
    | show_command Quit = "Quit"

  val rnd_col = Lcg.next_vector (Vector.fromList [Red, Green, Blue, Yellow, Purple, Brown])
  val rnd_bool = Lcg.next_vector (Vector.fromList [true, false])

  (** game utilities **)
  fun new_blocks x = pair (x, 2)

  fun add_blocks ((x, y), (b0, b1, b2)) f =
    let
      val cl = Vector.sub (f, x)
      val bs = [(y - 2, b0), (y - 1, b1), (y, b2)]
    in
      Vector.update (f, x, bs @ cl)
    end

  fun top_block f =
    pair f #> Vector.sub #> map fst #> foldl (uncurry min) height

  val move = apfst

  fun move_x f = move (apfst f)

  fun move_y f = move (apsnd f)

  fun drop fld =
    move (fn (x, _) => (x, top_block fld x |> dec))

  fun rot (c0, c1, c2) = (c1, c2, c0)

  val min_cluster = 3

  fun mark_line raw_l =
    let
      val l = sort (fn ((c0, _), (c1, _)) => c0 < c1) raw_l
      fun f ((coord, col), ((run_len, run_col, part), res)) =
        if col = run_col then
          ((inc run_len, run_col, coord::part), res)
        else
          ((1, col, [coord]),
           if run_len >= min_cluster then
             part @ res
           else
             res)
      fun append_last_part ((run_len, _, p), r) = if run_len >= min_cluster then r @ p else r
    in
      foldl f ((0, Red, []), []) l |> append_last_part
    end

  fun mark_vertical fld =
    fld
    |> Vector.mapi (fn (x, l) =>
      if x = 0 orelse x > width then
        []
      else
        mark_line l |> map (pair x))
    |> Vector.foldl (fn (l, res) => l @ res) []

  fun mark_clusters fld =
    let
      val vert = mark_vertical fld
      val hor = []
      val diag = []
    in
      vert @ hor @ diag
    end

  fun hit fld ((x, y), _) =
           x < 1
    orelse x > width
    orelse y >= height
    orelse y >= top_block fld x

  fun safe_move m f = try (not o (hit f)) m

  fun apply_ev f Right = (safe_move (move_x inc)) f
    | apply_ev f Left = (safe_move (move_x dec)) f
    | apply_ev f Drop = drop f
    | apply_ev _ Rotate = (apsnd rot)
    | apply_ev _ Quit = I

  fun new_colors st =
    let
      val (st1, c0) = rnd_col st
      val (st2, c1) = rnd_col st1
      val (st3, c2) = rnd_col st2
    in
      ((c0, c1, c2), st3)
    end

  (** main game step function **)
  fun tick (Landed (x, _)) _ fld st _ =
      let
        val (colors, st') = new_colors st
      in
        (Falling (new_blocks x colors), fld, st')
      end
    | tick (Falling c0) ev fld st do_drop =
      let
        val c1 as (p1, _) = maybe_apply (apply_ev fld) ev c0
        val c2 = move_y inc c1
      in
        if hit fld c2 then
          (Landed p1, add_blocks c1 fld, st)
        else
          (Falling (if do_drop then c2 else c1), fld, st)
      end

  (** utilities for rendering **)
  fun sdl_col Red        = {r=255, g=  0, b=  0, a=255}
    | sdl_col Green      = {r=  0, g=255, b=  0, a=255}
    | sdl_col Blue       = {r=  0, g=  0, b=255, a=255}
    | sdl_col Yellow     = {r=255, g=255, b=  0, a=255}
    | sdl_col Purple     = {r=160, g= 32, b=240, a=255} (* X11 Purple *)
    | sdl_col Brown      = {r=139, g= 69, b= 19, a=255} (* X11 Saddle Brown *)
    | sdl_col Black      = {r=100, g=100, b=100, a=255}
    | sdl_col Background = {r= 50, g= 50, b= 50, a=255}

  fun mk_rect x y = {x = size * x, y = size * y, w = size, h = size}

  (** actual rendering **)
  fun render_column r x =
    List.app
      (fn (y, col) =>
        (Sdl.set_render_draw_color r (sdl_col col);
         Sdl.render_fill_rect r (mk_rect x y)))

  fun render_field r =
    Vector.appi (uncurry (render_column r))

  fun render_mark r (x, y) =
    Sdl.render_fill_rect r (mk_rect x y)

  fun render_marks r col ms =
    (Sdl.set_render_draw_color r (sdl_col col);
     List.app (render_mark r) ms)

  fun populate_field (Falling b) = add_blocks b
    | populate_field (Landed _) = I

  fun render r f md ms =
    (Sdl.set_render_draw_color r (sdl_col Background);
     Sdl.render_clear r;
     populate_field md f |> render_field r;
     maybe_do (render_marks r Red) ms ();
     Sdl.render_present r)

  (** command queue - actually only stores one command **)
  val empty_cmd_queue = NONE

  fun push_cmd _ (SOME Quit) = SOME Quit
    | push_cmd x _ = SOME x

  fun queue_contains (SOME x) y = (x = y)
    | queue_contains _ _ = false

  (** getting events from SDL and parsing **)
  fun handle_event (Sdl.Key_Down k) =
      (case k
       of Sdl.K_Left  => push_cmd Left
        | Sdl.K_Right => push_cmd Right
        | Sdl.K_Up    => push_cmd Rotate
        | Sdl.K_Down  => push_cmd Drop
        | Sdl.K_Q     => push_cmd Quit
        | _           => I)
    | handle_event Sdl.Quit = push_cmd Quit
    | handle_event _ = I

  fun gather_commands q =
    case Sdl.poll_event ()
    of SOME {t, ...} => handle_event t q |> gather_commands
     | NONE => q

  (** initialization **)
  val empty_field = Vector.tabulate (width + 2, fn i =>
    if i = 0 orelse i > width then
      List.tabulate (inc height, rpair Black)
    else
      [(height, Black)])

  fun init () = let
      val w = Sdl.create_window "hashira"
                Sdl.Windowpos_Undefined Sdl.Windowpos_Undefined 800 600 []
      val r = Sdl.create_renderer w ~1 0
      val st = Lcg.seed 13 Lcg.nr
    in
      (r, st)
    end

  (** main game loop **)
  val tick_ms = 40
  val drop_frames = 10

  fun main () =
    let
      val (renderer, st) = init ()
      val first_column = Landed (1, 0)
      val start_ticks = Sdl.get_ticks ()
      fun run last_tick last_drop last_cmd_q md f st =
        let
          val this_ticks = Sdl.get_ticks ()
          val do_tick = this_ticks - last_tick >= tick_ms
          val cmd_q = gather_commands last_cmd_q
          val ((md', f', st'), last_tick', cmd_q', last_drop') =
            if do_tick then
              let val do_drop = inc last_drop >= drop_frames
              in
                (tick md cmd_q f st do_drop, this_ticks, empty_cmd_queue, if do_drop then 0 else inc last_drop)
              end
            else
              ((md, f, st), last_tick, cmd_q, last_drop)
          val (st'', bling) = rnd_bool st'
          val cs = if bling then SOME (mark_clusters f') else NONE
        in
          render renderer f' md' cs;
          Sdl.delay (tick_ms div 4);
          if queue_contains cmd_q Quit then
            ()
          else
            run last_tick' last_drop' cmd_q' md' f' st''
        end
    in
      run start_ticks 0 empty_cmd_queue first_column empty_field st
    end
end

(* vim: se ai et ts=2 sw=2: *)
