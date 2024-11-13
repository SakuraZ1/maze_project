# Overview 

The goal of this project is to develop a comprehensive Maze Generator and Solver application using OCaml. The application will enable users to generate mazes of varing sizes and complexities, visualize them, and solve them using different algorithms. 

Key Features:
  - Maze Generation:
     - Implement multiple maze generation algorithms
        - Recursive Backtracking
        - Prim's Algorithm
        - Kruskal's Algorithm

Maze Solve:
   - Implement multiple maze solver algorithms
         - BFS 
         - A-star search

## Mock
The Maze Game will have a GUI for users:
```
Maze Page ("/"):
    click "Generate" button to choose an algorithm to generate a random maze
    click "Up", "Down", "Left", "Right" button to move the charactor
        if the character reach the end, users can choose regenerate or see solutions
    click "solutions" button to choose the solving algorithm
        users can see the solution generated by the chosen algorithm
        the path of the solution will be visible on the screen
```  


## Libraries
```
Frontend: Rescript
Backend: Ocaml for algorithms, Dream for server
```

## Implementation Plan
```
Implement all non-trivial algorithms    Due Date: 11/17/2024
Implement server for HTTP requests      Due Date: 11/24/2024
Implement Frontend                      Due Date: 12/01/2024
```
