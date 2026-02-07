#include "Settings.h"

void Settings::clamp() {
  if (numPlayers < 0) numPlayers = 0;
  if (numPlayers > 2) numPlayers = 2;
  if (numBoards < kMinBoards) numBoards = kMinBoards;
  if (numBoards > kMaxBoards) numBoards = kMaxBoards;
}
