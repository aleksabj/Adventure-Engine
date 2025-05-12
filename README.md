# Text Game Engine

This project is a text adventure game engine written in Haskell. It allows defining a game world using a human-readable domain-specific language (DSL), and lets players explore that world via typed commands in the terminal.

The idea behind this engine is to support core features common in classic interactive fiction (like *Zork*), while focusing on functional design, clean state management, and simple extensibility.

### Key Features

- **Custom Game Description Format**  
  Worlds are defined in a structured plain-text file, listing locations, items, connections, and rules. 

- **Game State & Logic**  
  The engine tracks player position, inventory, object states (e.g. open/closed), and environmental flags. All state is modeled using immutable data structures.

- **Interactive Commands**  
  Players can type commands like `go north`, `take lamp`, `open trapdoor`, `read leaflet`, etc. A simple parser interprets these and updates the game state accordingly.

- **Game Mechanics Supported**
  - Openable containers (e.g. mailbox, bottle, trapdoor)
  - Nested items and inventory
  - Movable objects that reveal hidden paths (e.g. rug revealing a trapdoor)

- **Goal-Oriented Gameplay**
  Games can specify an end condition (e.g. reach a location while carrying a specific item), allowing simple quests to be defined.

### Project Goals

The project aims to:
- Explore functional programming techniques for stateful systems
- Build a minimal text-based game framework
- Separate world definition from logic using a DSL
- Provide a base that could later be extended

While the engine will not include rich storytelling or puzzles, it will support the technical foundations needed to build and play small interactive fiction games.

---

> This is a work in progress. The plan is to iteratively build parsing, world modeling, command interpretation, and a simple gameplay loop.
