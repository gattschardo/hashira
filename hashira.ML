signature HASHIRA =
sig
  datatype command = None | Left | Right | Drop
  datatype color = Red | Black | Background
  datatype mode = Falling of int * (int * color) list | Landed of int

  type field = (int * color) list vector

  val height : int
  val width : int

  val init : unit -> (Sdl.renderer * field)
  val tick : mode -> command -> field -> (mode * field)

  val render : field -> mode -> Sdl.renderer -> unit

  val main : unit -> unit
end

structure Hashira : HASHIRA =
struct
  datatype command = None | Left | Right | Drop
  datatype color = Red | Black | Background
  datatype mode = Falling of int * (int * color) list | Landed of int

  type field = (int * color) list vector

  val height = 12
  val width = 10
  val size = 40

  val empty_field = Vector.tabulate (width + 2, fn i =>
    if i = 0 orelse i > width then
      List.tabulate (height + 1, fn j => (j, Black))
    else
      [(height, Black)])

  fun init () = let
      val w = Sdl.create_window "hashira" Sdl.Windowpos_Undefined Sdl.Windowpos_Undefined 800 600 []
      val r = Sdl.create_renderer w ~1 0
    in
      (r, empty_field)
    end

  fun new_blocks c =
    [(0,c), (1,c), (2,c)]

  fun add_blocks bs i f =
    let
      val cl = Vector.sub (f, i)
      val cl' = List.concat [bs, cl]
    in
      Vector.update (f, i, cl')
    end

  fun apply_ev Right _ cl l = (print "right\n";(cl + 1, l))
    | apply_ev Left  _ cl l = (cl - 1, l)
    | apply_ev Drop _ cl l = (cl,l)
    | apply_ev None _ cl l = (cl,l)

  fun tick (Landed col) _ (fld:field) =
    (Falling (col, new_blocks Red), fld)
    | tick (Falling (cl, l)) ev (fld:field) =
      let
        val (cl', l') = apply_ev ev fld cl l
        fun drop (y,r) = (y + 1, r)
        val l'' = map drop l'
        fun max ((x,_),m) = if m > x then m else x
        val bot = foldl max 0 l''
        val target = Vector.sub (fld, cl')
        fun hit ((x,_),old) = old orelse (x - 1 = bot)
        val down = foldl hit false target
      in
        if down then
          (Landed cl', add_blocks l'' cl' fld)
        else
          (Falling (cl', l''), fld)
      end

  fun sdl_col Red = {r=100, g=0, b=0, a=255}
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
        _ => None)
    | handle_event _ = None
  fun main () =
    let
      val (r, f) = init ()
      fun run md f =
        let
          val ev = Sdl.poll_event ()
          val cmd =
            case ev of
              NONE => None |
              SOME ev => handle_event (#t ev)
          val (md', f') = tick md Drop f
        in
          render f' md' r;
          Sdl.delay 400;
          run md' f'
        end
    in
      run (Landed 1) f
    end
end

(* vim: se ai et: *)
