#pragma once

#include <string>
#include <utility>
#include <vector>

#include "Board.h"
#include "Settings.h"

struct BoardRect {
  int y{};
  int x{};
  int h{};
  int w{};
};

class UI {
public:
  UI();

  void drawMainMenu(int selected) const;
  void drawHelp() const;
  void drawSettings(const Settings& s, int selectedItem, int maxBoardsFit) const;

  // Dibuja el modo juego (tableros, selección y estadísticas).
  // cursors[i] = {row, col} seleccionado en el tablero i.
  void drawGame(const Settings& s,
                const std::vector<Board>& boards,
                const std::vector<std::pair<int, int>>& cursors,
                int selectedBoard,
                bool autoPaused,
                int maxBoardsFit) const;

  std::vector<BoardRect> layoutBoards(int count) const;
  int maxBoardsFit() const;

  static void drawCentered(int y, const std::string& text, int attr = 0);
};
