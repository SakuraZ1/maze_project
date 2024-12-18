open ReactDOM
open Fetch


/* Corrected sendRequest function */
let sendRequest = (
  url: string,
  method_: string,
  body: option<Js.Json.t>,
  callback: Js.Json.t => unit
) => {
  /* Convert the optional JSON body to a string, defaulting to an empty string */
  let bodyString =
    body
    ->Belt.Option.map(Js.Json.stringify)
    ->Belt.Option.getWithDefault("")

  /* Define headers as an array of tuples */
  let headers: array<(string, string)> = [("Content-Type", "application/json")]}

 /* Construct the RequestInit record using Fetch.RequestInit.make */
  let options = RequestInit.make(
    ~method_=Some(method_),
    ~headers=Some(headers),
    ~body=Some(bodyString),
    ()
  )

  /* Call Fetch.fetch with the URL and options */
  Fetch.fetch(url, options)
  ->Js.Promise.then_(res => res.json())
  ->Js.Promise.then_(callback)
  ->Js.Promise.catch(err => Js.log(err))


@react.component
let app = () => {
  let (mazeData, setMazeData) = React.useState(() => None);
  let (solutionPath, setSolutionPath) = React.useState(() => None);
  let (generatorType, setGeneratorType) = React.useState(() => "recursive_backtracker");

  let generateMaze = () => {
    sendRequest(`/generate?type=${generatorType}`, "GET", None, data => setMazeData(Some(data)));
  };

  let solveMaze = solver => {
    let url = switch solver {
      | "BFS" => "/solve/bfs"
      | "AStar" => "/solve/astar"
    };
    mazeData->Belt.Option.mapWithDefault(() => Js.log("No maze to solve"), maze => {
      sendRequest(url, "POST", Some(maze), data => setSolutionPath(Some(data)));
    });
  };

  let handleMove = direction => {
    Js.log2("Move requested:", direction);
    // Logic for step-based movement can be added here if required
  };

  let renderMaze = mazeData => {
    // Render maze visually
    <div className="maze">
      {mazeData["grid"]->Js.Array.map(row =>
        <div className="maze-row">
          {row->Js.Array.map(cell =>
            <div
              className={`maze-cell ${cell["walls"]->Js.Array.map(wall => wall["has_wall"])->Belt.Array.someTrue ? "wall" : "path"}`}
            ></div>
          )}
        </div>
      )}
    </div>
  };

  let renderPath = path => {
    // Overlay solution path on the maze
    <div className="path">
      {path["path"]->Js.Array.map(step =>
        <div className="path-step">{`(${step[0]}, ${step[1]})`}</div>
      )}
    </div>
  };

  <div>
    <div>
      <select value={generatorType} onChange={e => setGeneratorType(ReactEvent.Form.target(e)->ReactEvent.Form.value)}>
        <option value="recursive_backtracker">"Recursive Backtracker"</option>
        <option value="prim">"Prim's Algorithm"</option>
        <option value="kruskal">"Kruskal's Algorithm"</option>
      </select>
      <button onClick={_ => generateMaze()}>{"Generate Maze"}</button>
    </div>
    {
      switch mazeData {
      | None => <div>{"No Maze Yet"}</div>
      | Some(maze) => <div>
          {renderMaze(maze)}
          <button onClick={_ => solveMaze(`BFS`)}>{"BFS Solution"}</button>
          <button onClick={_ => solveMaze(`AStar`)}>{"A* Solution"}</button>
          <div>
            <button onClick={_ => handleMove("up")}>{"Move Up"}</button>
            <button onClick={_ => handleMove("down")}>{"Move Down"}</button>
            <button onClick={_ => handleMove("left")}>{"Move Left"}</button>
            <button onClick={_ => handleMove("right")}>{"Move Right"}</button>
          </div>
        </div>
      }
    }
    {
      switch solutionPath {
      | None => <div>{"No Solution Yet"}</div>
      | Some(path) => renderPath(path)
      }
    }
  </div>;
};

ReactDom.render(<app />, document.querySelector("#root"));
