open Tsdl
let shouldQuit () =
    let evt = Sdl.Event.create() in
        if Sdl.poll_event (Some evt) then
            match Sdl.Event.(enum (get evt typ)) with
                | `Quit -> true
                | _ -> false
        else
            false
    ;;


let render ( renderer : Sdl.renderer) =
    print_endline "test";
    match  ((Sdl.set_render_draw_color renderer 255 0 0 255)) with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e;
    | Ok () -> print_endline "setting the color succeeded";
    match  (Sdl.render_draw_point renderer 10 10) with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e;
    | Ok () -> print_endline "it was ok";
;;

let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    match Sdl.create_window ~w:(640*2) ~h:(480*2) "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok window ->
        match Sdl.create_renderer (window) with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok renderer -> 
            ignore (Sdl.render_set_scale renderer 0.0 0.0);
            let running = ref true in
            while !running do 
                let tmp = shouldQuit() in
                ignore(if tmp == true then ignore(running := false;));

                ignore(Sdl.set_render_draw_color renderer 255 255 255 255);
                ignore (Sdl.render_clear(renderer));

                ignore (render(renderer));
                ignore(Sdl.render_present (renderer));
                Sdl.delay (20l);
            done;

            Sdl.destroy_window window;
            Sdl.quit ();
            0

let () = if !Sys.interactive then () else exit (main ())

