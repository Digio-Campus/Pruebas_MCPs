#include "Settings.h"

void Settings::clampToMaxBoards(int maxBoards) {
  if (maxBoards < 1) maxBoards = 1;
  if (boards < 1) boards = 1;
  if (boards > maxBoards) boards = maxBoards;

  if (players != PlayersMode::Zero && players != PlayersMode::One && players != PlayersMode::Two) {
    players = PlayersMode::Two;
  }
}

std::string Settings::playersLabel(PlayersMode mode) {
  switch (mode) {
    case PlayersMode::Zero:
      return "0 (Auto aleatorio X/O)";
    case PlayersMode::One:
      return "1 (Manual X y O)";
    case PlayersMode::Two:
      return "2 (Tú=O, X automática)";
  }
  return "2 (Tú=O, X automática)";
}
