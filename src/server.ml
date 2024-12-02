open Dream
open Maze

let maze_to_html maze =
  let rows = Array.to_list maze in
  let row_to_html row =
    Array.fold_left
      (fun acc cell ->
        acc ^ match cell with
        | Maze.Wall -> "<div class='cell wall'></div>"
        | Maze.Path -> "<div class='cell path'></div>")
      "" row
  in
  String.concat "" (List.map (fun row -> "<div class='row'>" ^ row_to_html row ^ "</div>") rows)

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
          .wall { background-color: black; }
          .path { background-color: white; }
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
  @@ Dream.not_found
