open Tsdl

type pointxy = { x:int; y:int }

let points : pointxy list = [{x=5; y=5}; {x=15; y=5}; {x=5; y=15}; {x=15; y=15}]


let ( let* ) = Result.bind
let shouldQuit () =
    let evt = Sdl.Event.create() in
        if Sdl.poll_event (Some evt) then
            match Sdl.Event.(enum (get evt typ)) with
                | `Quit -> true
                | _ -> false
        else
            false
    ;;



let render (renderer : Sdl.renderer) (points : pointxy list) : unit =
    let render_point (point : pointxy) = ignore ((Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:point.x ~y:point.y ~w:5 ~h:5)))) in
  ignore(
      let* () = Sdl.set_render_draw_color renderer 255 255 255 255 in
  let* () = Sdl.render_clear renderer in
  let* () = Sdl.set_render_draw_color renderer 0 0 0 255 in
  let* () =
      ignore(List.map render_point points);
      Sdl.render_present renderer;
      Ok ()
  in 
  Ok ()
  )
  
;;

let screen (renderer : Sdl.renderer) (points : pointxy list) =
    let rec loop renderer points =
        if shouldQuit() != true then (

            Sdl.delay (50l);
            render renderer points;
            ignore(loop renderer points);
        )
    in

    loop renderer points;
;;


let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    match Sdl.create_window ~w:(640) ~h:(480) "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok window ->
        match Sdl.create_renderer (window) with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok renderer -> 

            screen renderer points;

            Sdl.destroy_window window;
            Sdl.quit ();
            0

let () = if !Sys.interactive then () else exit (main ())

