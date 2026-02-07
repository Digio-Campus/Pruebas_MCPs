#pragma once

#include <random>
#include <utility>
#include <vector>

#include "Board.h"
#include "Settings.h"
#include "UI.h"

class Game {
public:
  explicit Game(const Settings& s);

  // Ejecuta la partida. Devuelve al menú cuando el usuario sale.
  void run(const UI& ui);

private:
  Settings settings_;
  std::vector<Board> boards_;
  std::vector<std::pair<int, int>> cursors_;
  int selectedBoard_ = 0;

  bool autoPaused_ = false;
  std::mt19937 rng_;

  void ensureBoardCount();
  void applyAutoMoves();
  bool canHumanPlay(const Board& b) const;
  Board::Cell humanPieceFor(const Board& b) const;

  void resetBoard(int i);
  void resetAllBoards();
};
