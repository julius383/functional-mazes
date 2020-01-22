# functional-mazes

A functional approach to maze generation using the book
[Mazes for Programmers](http://www.mazesforprogrammers.com/) as a guide.

## Installation

* First install [stack](https://docs.haskellstack.org/en/stable/README/) then
  clone the repo. 
* Execute `stack build` in the project's directory.
* Run `stack run fmaze` and you should get a file `out.png` with the results
  of maze generation.

## Project Structure
```
  ├── functional-mazes.cabal
  ├── LICENSE
  ├── README.md
  ├── Setup.hs
  ├── src
  │   ├── Algos.hs      # where maze generation algorithms live
  │   ├── Draw.hs       # code for rendering mazes into images
  │   ├── Main.hs
  │   └── Maze.hs       # code for graph projection into mazes
  └── stack.yaml
```

