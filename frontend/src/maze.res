// ReScript Frontend for Maze Solver

open ReactDom;

let sendRequest = (url, callback) => {
  fetch(url)
  ->then(res => res.json())
  ->then(callback)
  ->catch(err => Js.log(err));
};

[@react.component]
let app = () => {
  let (mazeData, setMazeData) = React.useState(() => None);
  let (solutionPath, setSolutionPath) = React.useState(() => None);

  let generateMaze = () => {
    sendRequest("/generate", data => setMazeData(Some(data)));
  };

  let solveMaze = (solver) => {
    let url = switch solver {
      | `BFS => "/solve/bfs"
      | `AStar => "/solve/astar"
    };
    sendRequest(url, data => setSolutionPath(Some(data)));
  };

  let renderMaze = (mazeData) => {
    // Logic to render the maze visually from mazeData
    <div>"Maze Rendered Here"</div>
  };

  let renderPath = (path) => {
    // Logic to overlay solution path on the maze
    <div>"Path Rendered Here"</div>
  };

  <div>
    <button onClick={_ => generateMaze()}>"Generate Maze"</button>
    {
      switch mazeData {
      | None => <div>"No Maze Yet"</div>
      | Some(maze) => <div>
          {renderMaze(maze)}
          <button onClick={_ => solveMaze(`BFS)}>"BFS Solution"</button>
          <button onClick={_ => solveMaze(`AStar)}>"A* Solution"</button>
        </div>
      }
    }
    {
      switch solutionPath {
      | None => <div>"No Solution Yet"</div>
      | Some(path) => renderPath(path)
      }
    }
  </div>;
};

ReactDom.render(<app />, document.querySelector("#root"));
