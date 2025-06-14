# Text Game Engine

This project is a text adventure game engine written in Haskell. It allows defining a game world in a structured `.txt` file and play through it by typing commands in the terminal.

The goal was to practice functional programming and build a minimal, working base for classic interactive fiction (like *Zork*).


---


### Key Features
- Parses a game world described in plain `.txt` files.
- Keeps track of player location, inventory, and item states.
- Lets the player interact with the world by typing commands like:
  - `look`
  - `go north`
  - `take key`
  - `open chest`
  - `move rug`
  - `read note`
  - `help`

- Supports simple game mechanics like:
  - Containers that can be opened
  - Movable items that reveal other items
  - Locked paths that require an item to pass
  - A goal condition to win

---

### Project structure

- `app/Main.hs` — the entry point. It shows a menu to pick which `.txt` game to play.
- `src/`
  - `GameWorld.hs` — data types for locations, connections, items.
  - `Parser.hs` — reads the `.txt` file and builds the game world.
  - `Engine.hs` — handles player commands and game logic.
  - `GameLoop.hs` — runs the game loop: prompts input, updates state, checks for goal.

- `examples/` — example `.txt` game files:
  - `intro-escape.txt`
  - `library-secrets.txt`
  - `castle-treasure.txt`

---

### How to set up

1. Make sure you have:
    - **GHC** installed
    - **Cabal** installed

    On Mac:
    ```
    brew install ghc cabal-install
    ```
    On Linux:
    ```
    sudo apt update 
    sudo apt install ghc cabal-install
    ```


2. Build the project:
    ```
    cabal build
    ```

3. Run it:
    ```
    cabal run
    ```

4. Pick a game when the menu appears:
    ```
    Welcome to the Adventure Engine!
    Please choose a game to play:
      1) Intro Escape
      2) Library Secrets
      3) Castle Treasure
    ```

Just type `1` to play Intro Escape.

---

### Example: how to win Intro Escape

To finish Intro Escape, you can type this step by step:
  ```
  > open chest
  > take key
  > go north
  > look
  > move rug
  > open trapdoor
  > go down
  ```

This unlocks the hidden path and completes the goal.

Use `look` and `inventory` anytime to see where you are or what you have.
