(* Jeremiah Roise February 24, 2025*)
(* I write this spinning cube program in Ocaml because I wanted to learn Ocaml and there's no better way to learn than a massively overscoped project.*)
(*Inspired and instructed by https://www.youtube.com/watch?v=kdRJgYO1BJM&list=PLYmIsLVSssdIOn5J71CVBblPlXici1_2A&index=9*)
open Tsdl

type pointxy = { x:float; y:float; z:float }

let points : pointxy list ref = ref [{x=20.0; y=20.0; z=120.0}; {x=120.0; y=20.0; z=120.0}; {x=20.0; y=120.0; z=120.0}; {x=120.0; y=120.0; z=120.0}; {x=20.0; y=20.0; z=20.0}; {x=120.0; y=20.0; z=20.0}; {x=20.0; y=120.0; z=20.0}; {x=120.0; y=120.0; z=20.0}]

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
    let render_point (point : pointxy) = ignore ((Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:(int_of_float(floor point.x)) ~y:(int_of_float(floor point.y)) ~w:5 ~h:5)))) in
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

(* Transform moves all the points in a list by whatever the offset is for that dimension.*)
let transform points xoff yoff zoff =
    let transformPoint (point : pointxy) : pointxy = {x=(point.x +. xoff); y=(point.y +. yoff); z=(point.z +. zoff)} in
    List.map transformPoint points

(* This just transforms in the negative direction this was just the fastest way to implement.*)
let transformneg points xoff yoff zoff =
    let transformPoint (point : pointxy) : pointxy = {x=(point.x -. xoff); y=(point.y -. yoff); z=(point.z -. zoff)} in
    List.map transformPoint points


(* This funtion finds the centroid or average position of all the points passed into it*)
let generateCentroidOffset (points: pointxy list) =
    let size = float_of_int (List.length points) in
    let sum_list (lst : pointxy list) = List.fold_left (fun acc (x : pointxy) -> acc +. x.x) 0.0 lst in
    let offx = sum_list points /. size in
    let sum_list (lst : pointxy list) = List.fold_left (fun acc (x : pointxy) -> acc +. x.y) 0.0 lst in
    let offy = sum_list points /. size in
    let sum_list (lst : pointxy list) = List.fold_left (fun acc (x : pointxy) -> acc +. x.z) 0.0 lst in
    let offz = sum_list points /. size in
    let (return : pointxy) = {x=offx;y=offy;z=offz} in
    return



(* This is the magic part of the math that I don't fully understand yet.*)
let rotate (point : pointxy) xrad yrad zrad =
    let (x : float ref) =  ref point.x in 
    let (y : float ref) =  ref point.y in 
    let (z : float ref) =  ref point.z in 
    ignore (y := ((cos xrad) *. (!y)) -. ((sin xrad) *. !z));
    ignore (z := ((sin xrad) *. (!y)) +. ((cos xrad) *. !z));

    ignore (x := ((cos yrad) *. (!x)) +. ((sin yrad) *. !z));
    ignore (z := ((sin yrad) *. (!x)) +. ((cos yrad) *. !z));

    ignore (x := ((cos zrad) *. (!x)) -. ((sin zrad) *. !y));
    ignore (y := ((sin zrad) *. (!x)) +. ((cos zrad) *. !y));
    let return : pointxy = {x=(!x);y=(!y);z=(!z)} in
    return


let screen (renderer : Sdl.renderer) (points : pointxy list ref) =
    points := transform !points 200.0 100.0 0.0;
    let centroid = generateCentroidOffset !points in
    let rec loop renderer (points : pointxy list ref) =
        if shouldQuit() != true then (
            Sdl.delay (50l);
            points := transformneg !points centroid.x centroid.y centroid.z;
            let irot (point : pointxy) = rotate point 0.01 0.00 0.01 in 
            points := List.map irot !points;
            points := transform !points centroid.x centroid.y centroid.z;
            render renderer !points;
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

