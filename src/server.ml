
open Core
open Maze
open Cell

(* maze to html *)
let maze_to_html maze =
  let rows = Maze.get_grid maze in
  let row_to_html row =
    List.fold_left row ~init:"" ~f:(fun acc cell ->
      let walls = List.map cell.walls ~f:(fun (dir, has_wall) ->
        if has_wall then
          match dir with
          | Cell.North -> "border-top: 2px solid black;"
          | Cell.South -> "border-bottom: 2px solid black;"
          | Cell.East -> "border-right: 2px solid black;"
          | Cell.West -> "border-left: 2px solid black;"
        else ""
      ) in
      let style = String.concat ~sep:" " walls in
      acc ^ Printf.sprintf "<div class='cell' style='%s'></div>" style
    )
  in
  String.concat ~sep:"" (List.map rows ~f:(fun row -> "<div class='row'>" ^ row_to_html row ^ "</div>"))

let handler _req =
  let maze = Maze.create 20 20 in
  let maze_html = maze_to_html maze in
  let response_html =
    Printf.sprintf
      {|
      <!DOCTYPE html>
      <html>
      <head>
        <title>Maze</title>
        <style>
          body { font-family: Arial, sans-serif; display: flex; justify-content: center; align-items: center; height: 100vh; margin: 0; background-color: #f0f0f0; }
          .maze-container { display: inline-block; }
          .row { display: flex; }
          .cell { width: 20px; height: 20px; }
        </style>
      </head>
      <body>
        <div class="maze-container">
          %s
        </div>
      </body>
      </html>
      |}
      maze_html
  in

  Dream.html response_html

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" handler;
  ]
