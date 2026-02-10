#pragma once

#include <array>
#include <random>
#include <vector>

class Board {
public:
  enum class Cell { Empty, X, O };
  enum class Result { InProgress, WinX, WinO, Draw };

  Board();

  void reset();

  // Coloca en (r,c) la ficha del turno actual si está vacío.
  // Devuelve true si se realizó el movimiento.
  bool place(int r, int c);

  // Realiza un movimiento aleatorio válido para el turno actual.
  // Devuelve true si se realizó el movimiento.
  bool placeRandom(std::mt19937& rng);

  Cell at(int r, int c) const;

  Cell currentTurn() const { return turn_; }
  Result result() const { return result_; }

  int xWins() const { return xWins_; }
  int oWins() const { return oWins_; }
  int draws() const { return draws_; }

private:
  std::array<Cell, 9> cells_{};
  Cell turn_ = Cell::X;
  Result result_ = Result::InProgress;

  int xWins_ = 0;
  int oWins_ = 0;
  int draws_ = 0;

  void updateResultAndStats(Result prev);
  Result computeResult() const;
  bool isFull() const;
  std::vector<int> emptyIndices() const;

  static int idx(int r, int c) { return r * 3 + c; }
};
