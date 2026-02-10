#pragma once

#include <ncurses.h>

#include <string>

struct Layout {
  int marginX = 1;
  int marginY = 1;

  int boardW = 13;          // Ancho de la rejilla (ASCII)
  int boardGridH = 7;       // Alto de la rejilla (ASCII)
  int boardHeaderH = 2;     // Título + stats
  int boardH = 9;           // header + grid

  int padX = 2;
  int padY = 1;

  int footerH = 2;          // líneas reservadas abajo

  int cols = 1;
  int rows = 1;
  int maxBoards = 1;
};

class UI {
public:
  UI();
  ~UI();

  UI(const UI&) = delete;
  UI& operator=(const UI&) = delete;

  void refreshSize();

  int termH() const { return termH_; }
  int termW() const { return termW_; }

  Layout calcLayout() const;

  void clear();
  void drawCentered(int y, const std::string& text, int attrs = 0) const;

  // Pares de colores (si el terminal lo soporta).
  enum ColorPair {
    CP_NORMAL = 1,
    CP_HILITE = 2,
    CP_CURSOR = 3,
    CP_WIN = 4,
    CP_WARN = 5,
  };

  bool colorsEnabled() const { return colorsEnabled_; }

private:
  int termH_ = 0;
  int termW_ = 0;
  bool colorsEnabled_ = false;
};
