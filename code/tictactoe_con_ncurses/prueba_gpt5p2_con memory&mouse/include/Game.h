#pragma once

#include "Board.h"
#include "Settings.h"
#include "UI.h"

#include <random>
#include <vector>

class Game {
public:
  explicit Game(UI& ui) : ui_(ui) {}

  void run(Settings settings);

private:
  void ensureBoards(const Settings& settings);
  void clampSettingsToScreen(Settings& settings);

  void maybeRunBots(const Settings& settings);
  void draw(const Settings& settings);

  void resetSelected();
  void resetAll();

  void nextBoard(int delta);

  UI& ui_;
  std::vector<Board> boards_;

  int selectedBoard_ = 0;
  std::vector<int> cursorR_;
  std::vector<int> cursorC_;

  std::mt19937 rng_{std::random_device{}()};
  int lastAutoTickMs_ = 0;
};
