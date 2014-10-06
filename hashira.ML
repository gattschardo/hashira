signature HASHIRA =
sig
  datatype command = Left | Right | Drop
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
  datatype command = Left | Right | Drop
  datatype color = Red | Black | Background
  datatype mode = Falling of int * (int * color) list | Landed of int

  type field = (int * color) list vector

  val height = 12
  val width = 10
  val empty_field = Vector.tabulate (width, fn _ => [(height, Black)])

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

  fun tick (Landed col) _ (fld:field) =
    (Falling (col, new_blocks Red), fld)
    | tick (Falling (cl, l)) _ (fld:field) =
      let
        fun drop (y,c) = (y + 1, c)
        val l' = map drop l
        fun max ((x,_),m) = if m > x then m else x
        val bot = foldl max 0 l'
        val target = Vector.sub (fld, cl)
        fun hit ((x,_),old) = old orelse (x - 1 = bot)
        val down = foldl hit false target
      in
        if down then
          (Landed cl, add_blocks l' cl fld)
        else
          (Falling (cl, l'), fld)
      end

  fun sdl_col Red = {r=100, g=0, b=0, a=255}
    | sdl_col Black = {r=100, g=100, b=100, a=255}
    | sdl_col Background = {r=50, g=50, b=50, a=255}

  fun render_column x rend c =
    List.app (fn (y,col) =>
        (Sdl.set_render_draw_color rend (sdl_col col);
        Sdl.render_fill_rect rend {x=40*x, y=40*y, w=40, h=40}))
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

  fun main () =
    let
      val (r, f) = init ()
      fun run md f =
        let
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
