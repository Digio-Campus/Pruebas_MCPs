#include "Settings.h"

void Settings::clamp() {
  boardsCount = std::clamp(boardsCount, 1, 24);

  int pm = static_cast<int>(playersMode);
  pm = std::clamp(pm, 0, 2);
  playersMode = static_cast<PlayersMode>(pm);
}
