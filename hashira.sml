signature HASHIRA =
sig
  type command
  type color

  type coord
  type position
  type field
  type state
  type cluster

  datatype mode = Falling of cluster | Landed of position

  val height : coord
  val width : coord
  
  val rnd_col : Lcg.t -> Lcg.t * color
  val new_blocks : coord -> color * color * color -> cluster
  val drop : field -> cluster -> cluster
  val rot : color * color * color -> color * color * color

  val init : unit -> (Sdl.renderer * state)
  val handle_event : Sdl.event_type -> command option
  val apply_ev : field -> command -> cluster -> cluster
  val tick : mode -> command option -> field -> state -> (mode * field * state)

  val render : Sdl.renderer -> field -> mode -> unit

  val main : unit -> unit
end

structure Hashira : HASHIRA =
struct
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

  val rnd_col = Lcg.next_vector (Vector.fromList [Red, Green, Blue, Yellow, Purple, Brown])

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

  fun tick (Landed (x, _)) _ fld st =
      let
        val (colors, st') = new_colors st
      in
        (Falling (new_blocks x colors), fld, st')
      end
    | tick (Falling c0) ev fld st =
      let
        val (c1 as (p1, _)) = maybe_apply (apply_ev fld) ev c0
        val c2 = move_y inc c1
      in
        if hit fld c2 then
          (Landed p1, add_blocks c1 fld, st)
        else
          (Falling c2, fld, st)
      end

  fun sdl_col Red        = {r=255, g=  0, b=  0, a=255}
    | sdl_col Green      = {r=  0, g=255, b=  0, a=255}
    | sdl_col Blue       = {r=  0, g=  0, b=255, a=255}
    | sdl_col Yellow     = {r=255, g=255, b=  0, a=255}
    | sdl_col Purple     = {r=160, g= 32, b=240, a=255} (* X11 Purple *)
    | sdl_col Brown      = {r=139, g= 69, b= 19, a=255} (* X11 Saddle Brown *)
    | sdl_col Black      = {r=100, g=100, b=100, a=255}
    | sdl_col Background = {r= 50, g= 50, b= 50, a=255}

  fun get_rect x y = {x = size * x, y = size * y, w = size, h = size}

  fun render_column r x =
    List.app
      (fn (y, col) =>
        (Sdl.set_render_draw_color r (sdl_col col);
         Sdl.render_fill_rect r (get_rect x y)))

  fun render_field r =
    Vector.appi (uncurry (render_column r))

  fun render r f md =
    let val f' =
      case md
      of Falling b => add_blocks b f
       | Landed _ => f
    in
      (Sdl.set_render_draw_color r (sdl_col Background);
       Sdl.render_clear r;
       render_field r f';
       Sdl.render_present r)
    end

  fun handle_event (Sdl.Key_Down k) =
      (case k
       of Sdl.K_Left  => SOME Left
        | Sdl.K_Right => SOME Right
        | Sdl.K_Up    => SOME Rotate
        | Sdl.K_Down  => SOME Drop
        | Sdl.K_Q     => SOME Quit
        | _           => NONE)
    | handle_event _ = NONE

  fun next_event () =
      case Sdl.poll_event ()
      of SOME {t, ...} =>
        (case t 
         of Sdl.Key_Down _ => SOME t
          | _ => next_event ())
       | _ => NONE

  val first_column = Landed (1, 0)

  fun main () =
    let
      val (r, st) = init ()
      fun run md f st =
        let
          val cmd = case next_event ()
                    of SOME ev => handle_event ev
                     | _ => NONE
          val (md', f', st') = tick md cmd f st
        in
          render r f' md';
          Sdl.delay 400;
          if cmd <> SOME Quit then
            run md' f' st'
          else
            ()
        end
    in
      st |> run first_column empty_field
    end
end

(* vim: se ai et ts=2 sw=2: *)
