signature HASHIRA =
sig
  datatype command = None | Left | Right | Drop | Quit
  datatype color = Red | Green | Blue | Yellow | Purple | Brown | Black | Background
  datatype mode = Falling of int * (int * color) list | Landed of int

  type field = (int * color) list vector
  type state = Lcg.lcg

  val height : int
  val width : int

  val rnd_col : Lcg.lcg -> Lcg.lcg * color

  val init : unit -> (Sdl.renderer * field * state)
  val tick : mode -> command -> field -> state -> (mode * field * state)

  val render : field -> mode -> Sdl.renderer -> unit

  val main : unit -> unit
end

structure Hashira : HASHIRA =
struct
  datatype command = None | Left | Right | Drop | Quit
  datatype color = Red | Green | Blue | Yellow | Purple | Brown | Black | Background
  datatype mode = Falling of int * (int * color) list | Landed of int

  type field = (int * color) list vector
  type state = Lcg.lcg

  val height = 12
  val width = 10
  val size = 40

  val rnd_col = Lcg.next_vector (Vector.fromList [Red, Green, Blue, Yellow, Purple, Brown])

  val empty_field = Vector.tabulate (width + 2, fn i =>
    if i = 0 orelse i > width then
      List.tabulate (height + 1, fn j => (j, Black))
    else
      [(height, Black)])

  fun init () = let
      val w = Sdl.create_window "hashira" Sdl.Windowpos_Undefined Sdl.Windowpos_Undefined 800 600 []
      val r = Sdl.create_renderer w ~1 0
      val st = Lcg.seed 13 Lcg.nr
    in
      (r, empty_field, st)
    end

  fun new_blocks (c0, c1, c2) =
    [(0,c0), (1,c1), (2,c2)]

  fun add_blocks bs i f =
    let
      val cl = Vector.sub (f, i)
      val cl' = List.concat [bs, cl]
    in
      Vector.update (f, i, cl')
    end

  fun apply_ev Right _ cl l = (cl + 1, l)
    | apply_ev Left  _ cl l = (cl - 1, l)
    | apply_ev Drop _ cl l = (cl,l)
    | apply_ev None _ cl l = (cl,l)

  fun tick (Landed col) _ (fld:field) st0 =
      let
        val (st1, c0) = rnd_col st0
        val (st2, c1) = rnd_col st1
        val (st3, c2) = rnd_col st2
        val blocks = new_blocks (c0, c1, c2)
      in
        (Falling (col, blocks), fld, st3)
      end
    | tick (Falling (cl0, l0)) ev (fld:field) st =
      let
        val (cl1, l1) = apply_ev ev fld cl0 l0
        fun drop (y,r) = (y + 1, r)
        val l2 = map drop l1
        fun max ((x,_),m) = if m > x then m else x
        val bot = foldl max 0 l2
        val target = Vector.sub (fld, cl1)
        fun hit ((x,_),old) = old orelse (x - 1 = bot)
        val down = foldl hit false target
      in
        if down then
          (Landed cl1, add_blocks l2 cl1 fld, st)
        else
          (Falling (cl1, l2), fld, st)
      end

  fun sdl_col Red = {r=100, g=0, b=0, a=255}
    | sdl_col Green = {r=0, g=100, b=0, a=255}
    | sdl_col Blue = {r=0, g=0, b=100, a=255}
    | sdl_col Yellow = {r=0, g=100, b=100, a=255}
    | sdl_col Purple = {r=100, g=0, b=100, a=255}
    | sdl_col Brown = {r=100, g=100, b=0, a=255}
    | sdl_col Black = {r=100, g=100, b=100, a=255}
    | sdl_col Background = {r=50, g=50, b=50, a=255}

  fun render_column x rend c =
    List.app (fn (y,col) =>
        (Sdl.set_render_draw_color rend (sdl_col col);
        Sdl.render_fill_rect rend {x=size*x, y=size*y, w=size, h=size}))
      c

  fun render_field f r =
    Vector.appi (fn (i,c) => render_column i r c) f

  fun render f md r =
    (
    Sdl.set_render_draw_color r (sdl_col Background);
    Sdl.render_clear r;
    render_field f r;
    case md of
      Falling (cl,l) => render_column cl r l |
      Landed _ => ();
    Sdl.render_present r)

  fun handle_event (Sdl.Key_Down k) =
      (case k of
        Sdl.Key_Left => Left |
        Sdl.Key_Right => Right |
        Sdl.Key_D => Drop |
        Sdl.Key_Q => Quit |
        _ => None)
    | handle_event _ = None

  fun next_event () =
    case Sdl.poll_event () of
      NONE => NONE |
      SOME ev => (case (#t ev) of
        Sdl.Key_Down _ => SOME ev |
        _ => next_event ())

  fun main () =
    let
      val (r, f, st) = init ()
      fun run md f st =
        let
          val ev = next_event ()
          val cmd =
            case ev of
              NONE => None |
              SOME ev => handle_event (#t ev)
          val (md', f', st') = tick md cmd f st
        in
          render f' md' r;
          Sdl.delay 400;
          if cmd <> Quit then
            run md' f' st'
          else
            ()
        end
    in
      run (Landed 1) f st
    end
end

(* vim: se ai et: *)
