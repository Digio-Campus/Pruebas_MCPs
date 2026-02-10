#pragma once

#include <string>

enum class PlayersMode { Zero = 0, One = 1, Two = 2 };

struct Settings {
  PlayersMode players = PlayersMode::Two;
  int boards = 4;

  void clampToMaxBoards(int maxBoards);

  static std::string playersLabel(PlayersMode mode);
};
