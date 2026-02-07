#pragma once

#include <algorithm>

enum class PlayersMode {
  Zero = 0, // auto X/O aleatorio
  One = 1,  // humano controla X y O
  Two = 2,  // humano controla O, X auto
};

class Settings {
public:
  PlayersMode playersMode = PlayersMode::Two;
  int boardsCount = 2;

  void clamp();
};
