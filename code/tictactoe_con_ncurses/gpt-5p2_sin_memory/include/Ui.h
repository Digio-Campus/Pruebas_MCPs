#pragma once

#include "Board.h"
#include "Settings.h"

#include <vector>

// Encapsula toda la UI en ncurses (menús, ajustes, ayuda y juego).
class Ui {
public:
  explicit Ui(Settings &settings);
  int run();

private:
  Settings &settings_;

  int mainMenu();
  void settingsMenu();
  void helpScreen();
  void gameLoop();

  // Render y layout
  void drawCenteredText(int y, const char *text, bool bold = false);
  void drawFrameTitle(const char *title);

  struct Layout {
    int cols = 1;
    int rows = 1;
    int boardW = 18;
    int boardH = 8;
    int marginTop = 2;
    int marginLeft = 1;
    int gapX = 2;
    int gapY = 1;
    bool fits = true;
  };

  Layout computeLayout(int termH, int termW, std::size_t boards) const;
  void drawBoards(const std::vector<Board> &boards, std::size_t currentBoard,
                  std::size_t selR, std::size_t selC);
  void drawSingleBoard(int y, int x, int idx, const Board &b, bool active,
                       std::size_t selR, std::size_t selC);

  // Utilidades de entrada
  void handleResize();
};
