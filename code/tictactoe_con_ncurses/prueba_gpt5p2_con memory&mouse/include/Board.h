#pragma once

#include <array>
#include <optional>
#include <random>
#include <utility>
#include <vector>

class Board {
public:
  enum class Result { InProgress, XWins, OWins, Draw };

  Board();

  // Coloca una ficha en (r,c) usando el turno actual del tablero.
  // Devuelve false si la jugada es inválida o el tablero ya terminó.
  bool makeMove(int r, int c);

  // Realiza una jugada completamente aleatoria (respetando el turno actual).
  // Devuelve false si no hay movimientos posibles.
  bool makeRandomMove(std::mt19937& rng);

  void reset();

  char cell(int r, int c) const;
  char currentTurn() const { return turn_; }
  Result result() const { return result_; }
  bool isFinished() const { return result_ != Result::InProgress; }

  int xWins() const { return xWins_; }
  int oWins() const { return oWins_; }
  int draws() const { return draws_; }

  std::optional<std::array<std::pair<int, int>, 3>> winningLine() const { return winningLine_; }

private:
  bool isCellEmpty(int r, int c) const;
  std::vector<std::pair<int, int>> emptyCells() const;
  void updateResultAfterMove(char placed);

  std::array<std::array<char, 3>, 3> grid_{};
  char turn_ = 'X';
  Result result_ = Result::InProgress;
  std::optional<std::array<std::pair<int, int>, 3>> winningLine_;

  int xWins_ = 0;
  int oWins_ = 0;
  int draws_ = 0;
};
