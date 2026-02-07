#pragma once

#include <cstddef>

struct Settings {
  // 0 = auto (X y O aleatorio), 1 = manual (X y O manual), 2 = humano(O) + auto(X)
  int numPlayers = 2;
  std::size_t numBoards = 4;

  static constexpr std::size_t kMinBoards = 1;
  static constexpr std::size_t kMaxBoards = 12;

  void clamp();
};
