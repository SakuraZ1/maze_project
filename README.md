## Project

Maze Generator and Solver
- Generate mazes with different algorithms, including Recursive Backtracking, Prim’s algorithm, Kruskal’s algorithm.
- Solve mazes visually using BFS and A* algorithms.
- Display mazes and solved path.

## Usage


Run commands:

- dune build
- dune exec ./server/print_maze.exe

After running above commands, the Command-line Interface (CLI) should be displayed:
- Enter maze width (positive integer):
    - Input the expected width of the maze
    - The range of the input is from 3 to 19
        - Too large or small is not good for viewing
        - If the input is out of the range, the error message should be displayed:
            - "Invalid input: Please enter a positive integer and be in > 2 and < 20, Or you will not see a nice Maze!"
- Enter maze height (positive integer):
    - Input the expected width of the maze
    - The range of the input is from 3 to 19
        - Too large or small is not good for viewing
        - If the input is out of the range, the error message should be displayed:
            - "Invalid input: Please enter a positive integer and be in > 2 and < 20, Or you will not see a nice Maze!"
- Choose maze generation algorithm (Kruskal, Prim, Recursive):
    - Choose the expected generator algorithm from "Kruskal", "Prim", and "Recursive"
        - Uppercase or lowercase are all fine
        - If the input is not matched with these three algorithms
            - The error message should be displayed:
                - "Invalid choice. Please select a valid algorithm."
            - Wait for users to input a correct algorithm
    - Note: for Maze generated, the entrey is on the top-left corner and exit is one the bottom-right corner
- Choose maze solving algorithm (BFS, AStar):
    - Choose the expected generator algorithm from "BFS" and "AStar"
        - Uppercase or lowercase are all fine
        - If the input is not matched with these three algorithms
            - The error message should be displayed:
                - "Invalid choice. Please select a valid algorithm."
            - Wait for users to input a correct algorithm


          
### Sample Input:
```
Enter maze width (positive integer): 5
Enter maze height (positive integer): 5
Choose maze generation algorithm (Kruskal, Prim, Recursive): RECUrsive
Choose maze solving algorithm (BFS, AStar): BFs
```
### Sample Output:
```
Generated Maze (recursive algorithm):
+---+---+---+---+---+
    |       |       |
+   +   +   +   +   +
|   |   |       |   |
+   +   +---+---+   +
|   |       |   |   |
+   +---+   +   +   +
|       |   |   |   |
+---+   +   +   +   +
|           |       |
+---+---+---+---+   +

Solution (with BFS path):
+---+---+---+---+---+
| ■ | ■ | ■ | ■ | ■ |
+---+---+---+---+---+
| ■ | ■ | ■ | ■ | ■ |
+---+---+---+---+---+
| ■ | ■ | ■ |   | ■ |
+---+---+---+---+---+
| ■ | ■ | ■ |   | ■ |
+---+---+---+---+---+
|   | ■ | ■ |   | ■ |
+---+---+---+---+---+
```

The above sample maze is generated by the Recursive Backtracking Algorithm, and solved by the BFS.
