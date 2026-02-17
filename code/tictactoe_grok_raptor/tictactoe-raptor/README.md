TicTacToe - ncurses (generated)

Features
- C++17, modular (.h/.cpp)
- ncurses UI with keyboard + mouse support (mousemask, KEY_MOUSE, getmouse)
- 3 modes: 0 (auto), 1 (single player controls X and O), 2 (player controls O, X auto)
- Multiple independent boards (1..9) with separate state and statistics
- Hit-testing / coordinate -> cell mapping implemented and unit-tested
- Resizes on terminal `KEY_RESIZE`

Build & run

  cd code/tictactoe-ncurses-generated
  make
  make run

Tests
  make test    # runs unit and a basic integration smoke test

Controls (in-game)
- Mouse: click a cell to place (depends on mode)
- Tab / keys 1-9: change selected board
- r: reset selected board, R: reset all boards
- h: help, q: quit

Design notes
- Uses enum class for `Cell` and `Result`
- Uses std::mt19937 for randomness
- UI layout & hit-testing are pure functions where possible (unit-tested)
- Makefile includes `test` target (unit + integration smoke test)
