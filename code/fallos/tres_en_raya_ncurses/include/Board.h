#pragma once

#include <array>
#include <random>
#include <string>

class Board {
public:
  enum class Outcome { InProgress, XWin, OWin, Draw };

  Board();

  void reset();

  // Coloca la ficha correspondiente al turno actual en la casilla idx (0..8).
  // Devuelve false si la jugada no es válida.
  bool placeAt(int idx);

  // Fuerza un movimiento aleatorio para el turno actual (si hay huecos y está en progreso).
  // Devuelve false si no se pudo mover.
  bool randomMove(std::mt19937 &rng);

  Outcome outcome() const { return outcome_; }
  char turn() const { return turn_; }

  const std::array<char, 9> &cells() const { return cells_; }

  int xWins() const { return xWins_; }
  int oWins() const { return oWins_; }
  int draws() const { return draws_; }

  std::string outcomeText() const;

private:
  void advanceTurn();
  void updateOutcomeAndStatsIfFinished();

  static Outcome evaluate(const std::array<char, 9> &cells);

  std::array<char, 9> cells_{};
  char turn_ = 'X';
  Outcome outcome_ = Outcome::InProgress;
  bool statsCounted_ = false;

  int xWins_ = 0;
  int oWins_ = 0;
  int draws_ = 0;
};
