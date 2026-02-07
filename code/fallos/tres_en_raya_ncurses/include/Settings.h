#pragma once

#include <cstdint>

struct Settings {
  // 0: autoplay (X y O aleatorias)
  // 1: un jugador controla X y O manualmente
  // 2: jugador controla O, X automática
  int numPlayers = 2;

  // Número de tableros simultáneos en pantalla.
  int numBoards = 4;

  // Semilla opcional (0 => semilla por tiempo). Se deja aquí por si se quiere reproducibilidad.
  std::uint32_t rngSeed = 0;

  static constexpr int kMinBoards = 1;
  static constexpr int kMaxBoards = 24;
};
